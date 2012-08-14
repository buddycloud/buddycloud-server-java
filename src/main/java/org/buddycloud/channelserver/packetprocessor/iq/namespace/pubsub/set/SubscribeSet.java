package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.concurrent.BlockingQueue;


import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubSet;
import org.buddycloud.channelserver.queue.statemachine.Subscribe;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class SubscribeSet implements PubSubElementProcessor {

    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    public SubscribeSet(BlockingQueue<Packet> outQueue, DataStore dataStore) {
        this.outQueue = outQueue;
        this.dataStore = dataStore;
    }
    
    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        String node = elm.attributeValue("node");
        
        if(node == null || node.equals("")) {

            /*
                7.2.3.3 NodeID Required
                
                <iq type='error'
                    from='pubsub.shakespeare.lit'
                    to='hamlet@denmark.lit/elsinore'
                    id='retract1'>
                  <error type='modify'>
                    <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                    <nodeid-required xmlns='http://jabber.org/protocol/pubsub#errors'/>
                  </error>
                </iq>
            */
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            
            Element badRequest = new DOMElement("bad-request",
                                                new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

            Element nodeIdRequired = new DOMElement("nodeid-required",
                                                    new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
            
            Element error = new DOMElement("error");
            error.addAttribute("type", "modify");
            error.add(badRequest);
            error.add(nodeIdRequired);
            
            reply.setChildElement(error);
            
            outQueue.put(reply);
            return;
        }
        
        JID subscribingJID        = reqIQ.getFrom();
        boolean isLocalNode       = dataStore.isLocalNode(node);
        boolean isLocalSubscriber = false;
        
        if(actorJID != null) {
            subscribingJID = actorJID;
        } else {
            
            isLocalSubscriber = dataStore.isLocalUser(subscribingJID.toBareJID());
            
            // Check that user is registered.
            if(!isLocalSubscriber) {
                
                // If the packet did not have actor, and the sender is not a local user
                // subscription is not allowed.
                
                /*
                   <iq type='error'
                       from='pubsub.shakespeare.lit'
                       to='hamlet@denmark.lit/elsinore'
                       id='create1'>
                     <error type='auth'>
                       <registration-required xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                     </error>
                   </iq>
                */
                
                IQ reply = IQ.createResultIQ(reqIQ);
                reply.setType(Type.error);
                PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.registration_required, 
                                                 org.xmpp.packet.PacketError.Type.auth);
                reply.setError(pe);
                outQueue.put(reply);
                return;
                
            }
        }
        
        //   6.1.3.1 JIDs Do Not Match
        String jid = elm.attributeValue("jid");
        if( !subscribingJID.toBareJID().equals(jid) ) {
            
            /*
             // 6.1.3.1 JIDs Do Not Match
             <iq type='error'
                 from='pubsub.shakespeare.lit'
                 to='francisco@denmark.lit/barracks'
                 id='sub1'>
               <error type='modify'>
                 <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                 <invalid-jid xmlns='http://jabber.org/protocol/pubsub#errors'/>
               </error>
             </iq>
             
            */
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            
            Element badRequest = new DOMElement("bad-request",
                                                new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

            Element nodeIdRequired = new DOMElement("invalid-jid",
                                                    new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
            
            Element error = new DOMElement("error");
            error.addAttribute("type", "wait");
            error.add(badRequest);
            error.add(nodeIdRequired);
            
            reply.setChildElement(error);
            
            outQueue.put(reply);
            return;
        }
        
        
        if(!isLocalNode) {
            
            if(isLocalSubscriber) {
                // Start process to subscribe to external node.
                Subscribe sub = Subscribe.buildSubscribeStatemachine(node, reqIQ, dataStore);
                outQueue.put(sub.nextStep());
                return;
            }
            
            // Foreign client is trying to subscribe on a node that does not exists.
            
            /*
                6.1.3.12 Node Does Not Exist
            
                <iq type='error'
                        from='pubsub.shakespeare.lit'
                        to='francisco@denmark.lit/barracks'
                        id='sub1'>
                      <error type='cancel'>
                        <item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                      </error>
                </iq>
             */
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.item_not_found, 
                                             org.xmpp.packet.PacketError.Type.cancel);
            reply.setError(pe);
            outQueue.put(reply);
            return;
        }
        
        // If node is whitelist
        //   6.1.3.4 Not on Whitelist
        
        // Subscribe to a node.
        
        NodeSubscriptionImpl nodeSubscription = dataStore.getUserSubscriptionOfNode(subscribingJID.toBareJID(), 
                                                                                node);
        String possibleExistingAffiliation  = nodeSubscription.getAffiliation();
        String possibleExistingSusbcription = nodeSubscription.getSubscription();
        if(org.buddycloud.channelserver.pubsub.affiliation.Affiliations.outcast.toString().equals(possibleExistingAffiliation)) {
             /*   
                  6.1.3.8 Blocked
                  <iq type='error'
                      from='pubsub.shakespeare.lit'
                      to='francisco@denmark.lit/barracks'
                      id='sub1'>
                    <error type='auth'>
                      <forbidden xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                    </error>
                  </iq>
              
              */
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.forbidden, 
                                             org.xmpp.packet.PacketError.Type.auth);
            reply.setError(pe);
            outQueue.put(reply);
            return;
            
        }
        
        if(possibleExistingSusbcription != null) {
        
            /*
                 6.1.3.9 Too Many Subscriptions
                 <iq type='error'
                     from='pubsub.shakespeare.lit'
                     to='francisco@denmark.lit/barracks'
                     id='sub1'>
                   <error type='wait'>
                     <policy-violation xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                     <too-many-subscriptions xmlns='http://jabber.org/protocol/pubsub#errors'/>
                   </error>
                 </iq> 
             */
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            
            Element badRequest = new DOMElement("policy-violation",
                                                new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

            Element nodeIdRequired = new DOMElement("too-many-subscriptions",
                                                    new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
            
            Element error = new DOMElement("error");
            error.addAttribute("type", "wait");
            error.add(badRequest);
            error.add(nodeIdRequired);
            
            reply.setChildElement(error);
            
            outQueue.put(reply);
            return;
            
        }
        
        // Finally subscribe to the node :-)
        HashMap<String, String> nodeConf = dataStore.getNodeConf(node);
        String defaultAffiliation = nodeConf.get(Conf.DEFUALT_AFFILIATION);
        String defaultSubscription = org.buddycloud.channelserver.pubsub.subscription.Subscriptions.unconfigured.toString();

        dataStore.subscribeUserToNode(subscribingJID.toBareJID(), 
                                      node, 
                                      defaultAffiliation, 
                                      defaultSubscription,
                                      isLocalSubscriber ? null : reqIQ.getFrom().getDomain());
        
        IQ reply = IQ.createResultIQ(reqIQ);
        Element pubsub = reply.setChildElement(PubSubSet.ELEMENT_NAME, JabberPubsub.NAMESPACE_URI);
        pubsub.addElement("subscription")
              .addAttribute("node", node)
              .addAttribute("jid", subscribingJID.toBareJID())
              .addAttribute("subscription", defaultSubscription);
        pubsub.addElement("affiliation")
              .addAttribute("node", node)
              .addAttribute("jid", subscribingJID.toBareJID())
              .addAttribute("affiliation", defaultAffiliation);
        
        outQueue.put(reply);
        
        /*
           TODO, send affiliation change message here too.
           8.9.4 Notifying Entities
           <message from='pubsub.shakespeare.lit' to='polonius@denmark.lit'>
              <pubsub xmlns='http://jabber.org/protocol/pubsub'>
                <affiliations node='princely_musings'>
                  <affilation jid='polonius@denmark.lit' affiliation='none'/>
                </affiliations>
              </pubsub>
           </message>
           
           <message type="headline" to="comptest.xmpp.lobstermonster.org" from="channels.buddycloud.org">
               <event xmlns="http://jabber.org/protocol/pubsub#event">
                  <subscription jid="tuomas@xmpp.lobstermonster.org" subscription="subscribed" node="/user/tuomas@buddycloud.org/posts"/>
               </event>
           </message>
           
           // check that foreign receivers are handled correctly.
        */
        
        Message msg = new Message();
        msg.setTo(reply.getTo());
        msg.setFrom(reply.getFrom());
        msg.setType(Message.Type.headline);
        Element subscription = msg.addChildElement("event", JabberPubsub.NS_PUBSUB_EVENT)
                                  .addElement("subscription");
        subscription.addAttribute("node", node)
                    .addAttribute("jid", subscribingJID.toBareJID())
                    .addAttribute("subscription", defaultSubscription);
        outQueue.put(msg);
        
        msg = new Message();
        msg.setTo(reply.getTo());
        msg.setFrom(reply.getFrom());
        msg.setType(Message.Type.headline);
        Element affiliation = msg.addChildElement("event", JabberPubsub.NS_PUBSUB_EVENT)
                                 .addElement("affiliation");
        affiliation.addAttribute("node", node)
                   .addAttribute("jid", subscribingJID.toBareJID())
                   .addAttribute("affiliation", defaultAffiliation);
        outQueue.put(msg);
    }

    @Override
    public boolean accept(Element elm) {
        return elm.getName().equals("subscribe");
    }

}
