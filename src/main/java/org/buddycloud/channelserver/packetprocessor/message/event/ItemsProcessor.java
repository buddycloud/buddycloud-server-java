package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Date;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.ItemNotFoundException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.utils.NotificationScheme;
import org.dom4j.Element;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class ItemsProcessor extends AbstractMessageProcessor {

    private static final Logger logger = Logger.getLogger(ItemsProcessor.class);

    public ItemsProcessor(BlockingQueue<Packet> outQueue, Properties configuration, ChannelManager channelManager) {
        super(channelManager, configuration, outQueue);
    }

    @Override
    public void process(Message packet) throws Exception {

        message = packet;
        node = message.getElement().element("event").element("items").attributeValue("node");

        if (true == channelManager.isLocalNode(node)) {
            return;
        }
        sendLocalNotifications(NotificationScheme.validSubscribers);
        handleItem();
    }

    private void handleItem() throws NodeStoreException {

        if (false == channelManager.nodeExists(node)) {
            channelManager.addRemoteNode(node);
        }
        Element item = message.getElement().element("event").element("items").element("item");
        Element entry = item.element("entry");
        if (null != entry) {
            handleNewItem(entry);
            return;
        }
    }

    private void deleteItem(String id) throws NodeStoreException {
        try {
            channelManager.deleteNodeItemById(node, id);
        } catch (ItemNotFoundException e) {
            logger.error("No item to delete, not a problem");
        }
    }

    private void handleNewItem(Element entry) throws NodeStoreException {
        try {
            String inReplyTo = null;
            Element reply;
            if (null != (reply = entry.element("in-reply-to"))) {
                inReplyTo = reply.attributeValue("ref");
            }
            Date updatedDate = Conf.parseDate(entry.elementText("updated"));
            deleteItem(entry.elementText("id"));
            NodeItemImpl nodeItem = new NodeItemImpl(node, GlobalItemIDImpl.toLocalId(entry.elementText("id")), updatedDate, entry.asXML(), inReplyTo);
            channelManager.addNodeItem(nodeItem);
        } catch (IllegalArgumentException e) {
            logger.error(e);
            return;
        }
    }
}
