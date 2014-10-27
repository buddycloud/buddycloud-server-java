package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.special.FirehoseGet;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class SpecialItemsGet extends PubSubElementProcessorAbstract {

    private static final String FIREHOSE = "/firehose";

    public SpecialItemsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);

        acceptedElementName = XMLConstants.ITEMS_ELEM;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        request = reqIQ;
        PubSubElementProcessor processor = null;
        node = elm.attributeValue(XMLConstants.NODE_ATTR);
        if (FIREHOSE.equals(node)) {
            processor = new FirehoseGet(outQueue, channelManager);
        } else {
            featureNotImplementedError();
            return;
        }
        processor.process(elm, actorJID, reqIQ, rsm);
    }

    private void featureNotImplementedError() throws InterruptedException {
        setErrorCondition(PacketError.Type.cancel, PacketError.Condition.feature_not_implemented);
        outQueue.put(response);
    }
}
