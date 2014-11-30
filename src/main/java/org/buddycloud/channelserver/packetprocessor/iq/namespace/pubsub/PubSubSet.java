package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.AffiliationEvent;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.ItemDelete;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeConfigure;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeCreate;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeDelete;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.Publish;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.SubscribeSet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.SubscriptionEvent;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.UnsubscribeSet;
import org.xmpp.packet.Packet;

public class PubSubSet extends PacketProcessorAbstract {

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

}
