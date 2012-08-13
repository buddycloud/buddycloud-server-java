package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.concurrent.BlockingQueue;


import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.queue.statemachine.Unsubscribe;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class UnsubscribeSet implements PubSubElementProcessor {

    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    public UnsubscribeSet(BlockingQueue<Packet> outQueue, DataStore dataStore) {
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

    @Override
    public boolean accept(Element elm) {
        return elm.getName().equals("unsubscribe");
    }

}
