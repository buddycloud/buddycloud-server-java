package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result.AffiliationsResult;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result.Configuration;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result.ItemsResult;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result.SubscriptionsResult;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.buddycloud.channelserver.queue.UnknownFederatedPacketException;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class PubSubResult extends PacketProcessorAbstract {

    private Logger LOGGER = Logger.getLogger(PubSubResult.class);
    private FederatedQueueManager federatedQueueManager;

    private final List<PubSubElementProcessorAbstract> elementProcessors = new LinkedList<PubSubElementProcessorAbstract>();

    public PubSubResult(BlockingQueue<Packet> outQueue, FederatedQueueManager federatedQueueManager, ChannelManager channelManager) {
        this.outQueue = outQueue;
        this.federatedQueueManager = federatedQueueManager;
        this.channelManager = channelManager;

        initElementProcessors();
    }

    private void initElementProcessors() {
        elementProcessors.add(new SubscriptionsResult(channelManager));
        elementProcessors.add(new AffiliationsResult(channelManager));
        elementProcessors.add(new ItemsResult(channelManager));
        elementProcessors.add(new Configuration(channelManager));
    }

    @Override
    public void process(IQ reqIQ) throws Exception {
        try {
            String node = federatedQueueManager.getRelatedNodeForRemotePacket(reqIQ);
            federatedQueueManager.passResponseToRequester(reqIQ.createCopy());

            Element pubsub = reqIQ.getChildElement();
            @SuppressWarnings("unchecked")
            List<Element> elements = pubsub.elements();

            boolean handled = false;
            for (Element x : elements) {
                for (PubSubElementProcessorAbstract elementProcessor : elementProcessors) {
                    if (elementProcessor.accept(x)) {
                        if (null != node) {
                            elementProcessor.setNode(node);
                        }
                        elementProcessor.process(x, null, reqIQ, x);
                        handled = true;
                    }
                }
            }
            if (handled) {
                return;
            }
        } catch (UnknownFederatedPacketException e) {
            LOGGER.error(e);
        }
    }

}
