package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.BlockingQueue;


import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ValidateEntry;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubSet;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.queue.statemachine.Publish;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class PublishSet implements PubSubElementProcessor {

    private static final Logger LOGGER = Logger.getLogger(PublishSet.class);
    
    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    public PublishSet(BlockingQueue<Packet> outQueue, DataStore dataStore) {
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
        
        NodeSubscriptionImpl nodeSubscription = dataStore.getUserSubscriptionOfNode(publishersJID.toBareJID(), 
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
        Element pubsub = new DOMElement(PubSubSet.ELEMENT_NAME, new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));
        
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
        
        Iterator<? extends NodeSubscription> cur = dataStore.getNodeSubscribers(node);
        while(cur.hasNext()) {
            NodeSubscription ns = cur.next();
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

    @Override
    public boolean accept(Element elm) {
        return elm.getName().equals("publish");
    }

}
