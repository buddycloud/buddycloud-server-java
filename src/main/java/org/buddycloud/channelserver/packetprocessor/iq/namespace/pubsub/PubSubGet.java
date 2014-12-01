package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.AffiliationsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.NodeConfigureGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.NodeThreadsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.RecentItemsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.RepliesGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.SubscriptionsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.SubscriptionsWithMetadataGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.ThreadGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.UserItemsGet;
import org.xmpp.packet.Packet;

public class PubSubGet extends PacketProcessorAbstract {

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
        elementProcessors.add(new SubscriptionsWithMetadataGet(outQueue, channelManager));
    }

}
