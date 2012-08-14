package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.BlockingQueue;


import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeCreate;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.PublishSet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.SubscribeSet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.UnsubscribeSet;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class PubSubSet implements PacketProcessor<IQ> {
    
    public static final String ELEMENT_NAME = "pubsub";

    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    private final List<PubSubElementProcessor> elementProcessors = new LinkedList<PubSubElementProcessor>();
    
    public PubSubSet(BlockingQueue<Packet> outQueue, DataStore dataStore) {
        this.outQueue = outQueue;
        this.dataStore = dataStore;
        initElementProcessors();
    }
    
    private void initElementProcessors() {
        elementProcessors.add(new PublishSet(outQueue, dataStore));
        elementProcessors.add(new SubscribeSet(outQueue, dataStore));
        elementProcessors.add(new UnsubscribeSet(outQueue, dataStore));
        elementProcessors.add(new NodeCreate(outQueue, dataStore));
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
        for (Element x : elements) {
            for (PubSubElementProcessor elementProcessor : elementProcessors) {
                if (elementProcessor.accept(x)) {
                    elementProcessor.process(x, actorJID, reqIQ, null);
                    handled = true;
                }
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
    
}
