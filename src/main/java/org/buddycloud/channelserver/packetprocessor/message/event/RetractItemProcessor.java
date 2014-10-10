package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.utils.NotificationScheme;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class RetractItemProcessor extends AbstractMessageProcessor {

    public RetractItemProcessor(BlockingQueue<Packet> outQueue,
            Properties configuration, ChannelManager channelManager) {
        super(channelManager, configuration, outQueue);
    }

    @Override
    public void process(Message packet) throws Exception {
        message = packet;
        node = message.getElement().element("event").element("items")
                .attributeValue("node");
        if (true == Configuration.getInstance().isLocalNode(node)) {
            return;
        }
        sendLocalNotifications(NotificationScheme.validSubscribers);
        handleItem();
    }

    private void handleItem() throws NodeStoreException {
        deleteItem(message.getElement().element("event").element("items")
                .element("retract").attributeValue("id"));
    }

    private void deleteItem(String id) throws NodeStoreException {
        channelManager.deleteNodeItemById(node, id);
    }
}
