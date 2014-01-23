package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.AffiliationEvent;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.ItemDelete;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeConfigure;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeCreate;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeDelete;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.Publish;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.SubscribeSet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.SubscriptionEvent;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.UnsubscribeSet;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class PubSubSet implements PacketProcessor<IQ> {
    
    public static final String ELEMENT_NAME = "pubsub";

    private static final Logger LOGGER = Logger.getLogger(PubSubSet.class);
    
    private final BlockingQueue<Packet> outQueue;
    private final ChannelManager channelManager;
    private final List<PubSubElementProcessor> elementProcessors = new LinkedList<PubSubElementProcessor>();
    
    public PubSubSet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        this.outQueue = outQueue;
        this.channelManager = channelManager;
        initElementProcessors();
    }
    
    private void initElementProcessors() {
        elementProcessors.add(new Publish(outQueue, channelManager));
        elementProcessors.add(new SubscribeSet(outQueue, channelManager));
        elementProcessors.add(new UnsubscribeSet(outQueue, channelManager));
        elementProcessors.add(new NodeCreate(outQueue, channelManager));
        elementProcessors.add(new NodeConfigure(outQueue, channelManager));
        elementProcessors.add(new SubscriptionEvent(outQueue, channelManager));
        elementProcessors.add(new AffiliationEvent(outQueue, channelManager));
        elementProcessors.add(new ItemDelete(outQueue, channelManager));
        elementProcessors.add(new NodeDelete(outQueue, channelManager));
    }
    
    @Override
    public void process(IQ reqIQ) throws Exception {
        
        Element pubsub = reqIQ.getChildElement();
        
        //Let's get the possible actor
        JID actorJID = null;
        if (pubsub.elementText("actor") != null) {
            actorJID = new JID(pubsub.elementText("actor").trim());
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
                    return;
                }
            }
        }
        
        IQ reply = IQ.createResultIQ(reqIQ);
        reply.setChildElement(reqIQ.getChildElement().createCopy());
        reply.setType(Type.error);
        PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.unexpected_request, 
                                         org.xmpp.packet.PacketError.Type.wait);
        reply.setError(pe);
        outQueue.put(reply);
    }
    
}
