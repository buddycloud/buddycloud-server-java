package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.channel.ValidateEntry;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.queue.statemachine.Publish;
import org.buddycloud.channelserver.queue.statemachine.Subscribe;
import org.buddycloud.channelserver.queue.statemachine.Unsubscribe;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.IQ.Type;

import com.mongodb.DBCursor;

public class PubSubSet implements PacketProcessor<IQ> {
    
    public static final String ELEMENT_NAME = "pubsub";
    private static final Logger LOGGER = Logger.getLogger(PubSubSet.class);

    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    public PubSubSet(BlockingQueue<Packet> outQueue, DataStore dataStore) {
        this.outQueue = outQueue;
        this.dataStore = dataStore;
    }
    
    @Override
    public void process(IQ reqIQ) throws Exception {
        
        Element pubsub = reqIQ.getChildElement();
        
        //Let's get the possible actor
        JID actorJID = null;
        if(pubsub.element("actor") != null) {
            actorJID = new JID(pubsub.element("actor").getTextTrim());
            /**
             * TODO validate here that the JID is somehow sane.
             *      We could check that the domains are the same etc.
             *      
             */
            // something like this:
            // reqIQ.getFrom().getDomain().contains(actorJID.getDomain());
            //
            // If not, return not-allowed or bad request?
            // <error type='cancel'>
            //    <not-allowed xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
            //
            //    or ?
            //
            //    <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
            // </error>
            //
            //actor = actorJID.toBareJID();
            
            if(!reqIQ.getFrom().getDomain().contains(actorJID.getDomain())) {
                IQ reply = IQ.createResultIQ(reqIQ);
                reply.setChildElement(reqIQ.getChildElement().createCopy());
                reply.setType(Type.error);
                PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.bad_request, 
                                                 org.xmpp.packet.PacketError.Type.cancel);
                reply.setError(pe);
                outQueue.put(reply);
                return;
            }
        }
        
        @SuppressWarnings("unchecked")
        List<Element> elements = pubsub.elements();

        boolean handled = false;
        String feature = "";
        for (Element x : elements) {
            feature = x.getName();
            if(feature.equals("subscribe")) {
                this.subscribe(x, actorJID, reqIQ);
                handled = true;
            } else if (feature.equals("publish")) {
                this.publish(x, actorJID, reqIQ);
                handled = true;
            } else if (feature.equals("unsubscribe")) {
                this.unsubscribe(x, actorJID, reqIQ);
                handled = true;
            }
            //break;
        }
        
        if (handled == false) {
            
            // <iq type='error'
            //     from='pubsub.shakespeare.lit'
            //     to='hamlet@denmark.lit/elsinore'
            //     id='create1'>
            //     <error type='cancel'>
            //       <feature-not-implemented xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
            //       <unsupported xmlns='http://jabber.org/protocol/pubsub#errors' feature='create-nodes'/>
            //     </error>
            // </iq>

            // TODO, fix this. Now we just reply unexpected_request.
            //       We should answer something like above.
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setChildElement(reqIQ.getChildElement().createCopy());
            reply.setType(Type.error);
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.unexpected_request, 
                                             org.xmpp.packet.PacketError.Type.wait);
            reply.setError(pe);
            outQueue.put(reply);
        }
    }
    
    private void unsubscribe(Element elm, JID actorJID, IQ reqIQ) throws Exception {
        
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
        
        JID unsubscribingJID      = reqIQ.getFrom();
        boolean isLocalNode       = dataStore.isLocalNode(node);
        boolean isLocalSubscriber = false;
        
        if(actorJID != null) {
            unsubscribingJID = actorJID;
        } else {
            
            isLocalSubscriber = dataStore.isLocalUser(unsubscribingJID.toBareJID());
            
            // Check that user is registered.
            if(!isLocalSubscriber) {
                
                // If the packet did not have actor, and the sender is not a local user
                // subscription is not allowed.
                
                /*
                   <iq type='error'
                       from='pubsub.shakespeare.lit'
                       to='hamlet@denmark.lit/elsinore'
                       id='unsub1'>
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
        
        if(!isLocalNode) {
            
            if(isLocalSubscriber) {
                //Start process to unsubscribe from external node.
                Unsubscribe unsub = Unsubscribe.buildUnsubscribeStatemachine(node, reqIQ, dataStore);
                outQueue.put(unsub.nextStep());
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

        dataStore.unsubscribeUserFromNode(unsubscribingJID.toBareJID(), node);
        
        IQ reply = IQ.createResultIQ(reqIQ);
        outQueue.put(reply);
        
    }
    
    private void subscribe(Element elm, JID actorJID, IQ reqIQ) throws Exception {
        
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
        
        NodeSubscription nodeSubscription = dataStore.getUserSubscriptionOfNode(subscribingJID.toBareJID(), 
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
        Element pubsub = reply.setChildElement(ELEMENT_NAME, JabberPubsub.NAMESPACE_URI);
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

    private void publish(Element elm, JID actorJID, IQ reqIQ) throws Exception {
        
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
        
        JID publishersJID         = reqIQ.getFrom();
        boolean isLocalNode       = dataStore.isLocalNode(node);
        boolean isLocalSubscriber = false;
        
        if(actorJID != null) {
            publishersJID = actorJID;
        } else {
            
            isLocalSubscriber = dataStore.isLocalUser(publishersJID.toBareJID());
            
            // Check that user is registered.
            if(!isLocalSubscriber) {
                
                // If the packet did not have actor, and the sender is not a local user.
                // publishing is not allowed.
                
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
        
        if(!isLocalNode) {
            
            if(isLocalSubscriber) {
                
                //TODO, WORK HERE!
                
                Publish pub = Publish.buildSubscribeStatemachine(node, reqIQ, dataStore);
                outQueue.put(pub.nextStep());
                return;
                // Start process to publish to external node.
                //Subscribe sub = Subscribe.buildSubscribeStatemachine(node, reqIQ, dataStore);
                //outQueue.put(sub.nextStep());
                //return;
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
        
        NodeSubscription nodeSubscription = dataStore.getUserSubscriptionOfNode(publishersJID.toBareJID(), 
                                                                                node);
        String possibleExistingAffiliation  = nodeSubscription.getAffiliation();
        String possibleExistingSusbcription = nodeSubscription.getSubscription();
        
        // TODO, check here that the publisher is allowed to publish!
        
        Element item = elm.element("item");
        if(item == null) {
            
            /*
              No item, let's reply something like this:
                <iq type='error'
                    from='pubsub.shakespeare.lit'
                    to='hamlet@denmark.lit/elsinore'
                    id='publish1'>
                  <error type='modify'>
                    <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                    <item-required xmlns='http://jabber.org/protocol/pubsub#errors'/>
                  </error>
                </iq>
             */
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            
            Element badRequest = new DOMElement("bad-request",
                                                new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

            Element nodeIdRequired = new DOMElement("item-required",
                                                    new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
            
            Element error = new DOMElement("error");
            error.addAttribute("type", "modify");
            error.add(badRequest);
            error.add(nodeIdRequired);
            
            reply.setChildElement(error);
            
            outQueue.put(reply);
            return;
        }
        
        ValidateEntry vEntry = new ValidateEntry(item.element("entry"));
        if(!vEntry.isValid()) {
            LOGGER.info("Entry is not valid: '" + vEntry.getErrorMsg() + "'.");
            
            /*
                 <iq type='error'
                     from='pubsub.shakespeare.lit'
                     to='hamlet@denmark.lit/elsinore'
                     id='publish1'>
                   <error type='modify'>
                     <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                     <invalid-payload xmlns='http://jabber.org/protocol/pubsub#errors'/>
                   </error>
                 </iq>
             */
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            
            Element badRequest = new DOMElement("bad-request",
                                                new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

            Element nodeIdRequired = new DOMElement("invalid-payload",
                                                    new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
            
            Element error = new DOMElement("error");
            error.addAttribute("type", "modify");
            error.add(badRequest);
            error.add(nodeIdRequired);
            
            reply.setChildElement(error);
            
            outQueue.put(reply);
            return;
        }
        
        Element entry = vEntry.createBcCompatible(publishersJID.toBareJID(), 
                                                  reqIQ.getTo().toBareJID(), 
                                                  node);
        
        String id = entry.element("id").getText();
        String[] idParts = id.split(",");
        id = idParts[2];
        
        //Let's store the new item.
        dataStore.storeEntry(node, id, entry.asXML());
        
        /* 
         * Success, let's reply as defined in 
         * http://xmpp.org/extensions/xep-0060.html#publisher-publish - 7.1.2 Success Case
         */
        Element pubsub = new DOMElement(ELEMENT_NAME, new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));
        
        Element publish = pubsub.addElement("publish");
        publish.addAttribute("node", node);
        
        Element newItem = publish.addElement("item");
        newItem.addAttribute("id", id);
        
        IQ result = IQ.createResultIQ(reqIQ);
        result.setChildElement(pubsub);
        
        outQueue.put(result);
        
        
        // Let's send notifications as defined in 7.1.2.1 Notification With Payload
        Message msg = new Message();
        msg.setType(Message.Type.headline);
        msg.setID(id);
        Element event = msg.addChildElement("event", JabberPubsub.NS_PUBSUB_EVENT);
        Element items = event.addElement("items");
        items.addAttribute("node", node);
        Element i = items.addElement("item");
        i.addAttribute("id", id);
        i.add(entry.createCopy());
        
        Set<String> externalChannelServerReceivers = new HashSet<String>();
        
        DBCursor cur = dataStore.getNodeSubscribers(node);
        while(cur.hasNext()) {
            NodeSubscription ns = (NodeSubscription) cur.next();
            String toBareJID = ns.getBareJID();
            if(ns.getForeignChannelServer() != null) {
                if( externalChannelServerReceivers.contains(ns.getForeignChannelServer()) ) {
                    continue;
                }
                externalChannelServerReceivers.add(ns.getForeignChannelServer());
                toBareJID = ns.getForeignChannelServer();
            }
            msg.setTo(toBareJID);
            outQueue.put(msg.createCopy());
        }
        
    }
    
    
}
