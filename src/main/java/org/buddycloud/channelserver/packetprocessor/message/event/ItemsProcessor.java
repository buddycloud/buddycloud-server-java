package org.buddycloud.channelserver.packetprocessor.message.event;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.ItemNotFoundException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result.ItemsResult;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.dom4j.Element;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class ItemsProcessor extends AbstractMessageProcessor {
	
	private static final Logger logger = Logger.getLogger(ItemsProcessor.class);
	private static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.S'Z'";
	private SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);

	public ItemsProcessor(BlockingQueue<Packet> outQueue,
			Properties configuration, ChannelManager channelManager) {
		super(channelManager, configuration, outQueue);
	}

	@Override
	public void process(Message packet) throws Exception {

		message = packet;
		node = message.getElement().element("event").element("items")
				.attributeValue("node");

		if (true == channelManager.isLocalNode(node))
			return;
		sendLocalNotifications();
		handleItem();
	}

	private void handleItem() throws NodeStoreException {
		
		if (false == channelManager.nodeExists(node))
			channelManager.addRemoteNode(node);
		Element item = message.getElement().element("event").element("items")
				.element("item");
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
			logger.debug("No item to delete, not a problem");
		}
	}

	private void handleNewItem(Element entry) throws NodeStoreException {
		try {
			Date updatedDate = sdf.parse(entry.elementText("updated"));
			deleteItem(entry.elementText("id"));
			NodeItemImpl nodeItem = new NodeItemImpl(node,
					entry.elementText("id"), updatedDate, entry.asXML());
			channelManager.addNodeItem(nodeItem);
		} catch (ParseException e) {
			logger.error(e);
			return;
		}
	}
}