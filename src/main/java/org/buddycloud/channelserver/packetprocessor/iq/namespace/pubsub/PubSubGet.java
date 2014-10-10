package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.AffiliationsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.NodeConfigureGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.NodeThreadsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.RecentItemsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.RepliesGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.SubscriptionsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.ThreadGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.UserItemsGet;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class PubSubGet implements PacketProcessor<IQ> {

    private final BlockingQueue<Packet> outQueue;
    private final ChannelManager channelManager;
    private final List<PubSubElementProcessor> elementProcessors = new LinkedList<PubSubElementProcessor>();

    public PubSubGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        this.outQueue = outQueue;
        this.channelManager = channelManager;
        initElementProcessors();
    }

    private void initElementProcessors() {
        elementProcessors.add(new SubscriptionsGet(outQueue, channelManager));
        elementProcessors.add(new AffiliationsGet(outQueue, channelManager));
        elementProcessors.add(new ItemsGet(outQueue, channelManager));
        elementProcessors.add(new ThreadGet(outQueue, channelManager));
        elementProcessors.add(new UserItemsGet(outQueue, channelManager));
        elementProcessors.add(new RecentItemsGet(outQueue, channelManager));
        elementProcessors.add(new NodeConfigureGet(outQueue, channelManager));
        elementProcessors.add(new NodeThreadsGet(outQueue, channelManager));
        elementProcessors.add(new RepliesGet(outQueue, channelManager));
    }

    @Override
    public void process(IQ reqIQ) throws Exception {

        Element pubsub = reqIQ.getChildElement();

        JID actorJID = null;
        if (pubsub.element("actor") != null) {
            actorJID = new JID(pubsub.element("actor").getTextTrim());
            /**
             * TODO(lloydwatkin) validate here that the JID is somehow sane. We could check that the
             * domains are the same etc.
             */
            // actor = actorJID.toBareJID();
        }

        // Let's get the possible rsm element
        Element rsm = pubsub.element(new QName("set", new Namespace("", "http://jabber.org/protocol/rsm")));

        @SuppressWarnings("unchecked")
        List<Element> elements = pubsub.elements();

        boolean handled = false;
        for (Element x : elements) {
            for (PubSubElementProcessor elementProcessor : elementProcessors) {
                if (elementProcessor.accept(x)) {
                    elementProcessor.process(x, actorJID, reqIQ, rsm);
                    handled = true;
                }
            }
        }

        if (!handled) {

            // <iq type='error'
            // from='pubsub.shakespeare.lit'
            // to='hamlet@denmark.lit/elsinore'
            // id='create1'>
            // <error type='cancel'>
            // <feature-not-implemented xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
            // <unsupported xmlns='http://jabber.org/protocol/pubsub#errors'
            // feature='create-nodes'/>
            // </error>
            // </iq>

            // TODO(lloydwatkin) fix this. Now we just reply unexpected_request.
            // We should answer something like above.

            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setChildElement(reqIQ.getChildElement().createCopy());
            reply.setType(Type.error);
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.unexpected_request, org.xmpp.packet.PacketError.Type.wait);
            reply.setError(pe);
            outQueue.put(reply);

        }
    }

}
