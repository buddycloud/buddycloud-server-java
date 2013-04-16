package org.buddycloud.channelserver.packetprocessor.message.event;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
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

public class ItemsProcessor implements PacketProcessor<Message> {

	private Message message;
	private String node;
	private ChannelManager channelManager;
	private Properties configuration;
	private BlockingQueue<Packet> outQueue;
	
	private static final Logger logger = Logger.getLogger(ItemsProcessor.class);
	private static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
	private SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);

	public ItemsProcessor(BlockingQueue<Packet> outQueue,
			Properties configuration, ChannelManager channelManager) {
		this.channelManager = channelManager;
		setConfiguration(configuration);
		this.outQueue = outQueue;
	}

	public void setConfiguration(Properties configuration) {
		this.configuration = configuration;
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
		Element retract = item.element("retract");
        if (null != entry) {
		    handleNewItem(entry);
		    return;
        }
        if (null != retract) {
        	deleteItem(retract.attributeValue("id"));
        	return;
        }
	}
	
	private void deleteItem(String id) throws NodeStoreException {
		channelManager.deleteNodeItemById(node, id);
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

	private void sendLocalNotifications() throws Exception {
		ResultSet<NodeSubscription> subscribers = channelManager
				.getNodeSubscriptions(node);
		for (NodeSubscription subscriber : subscribers) {
			message.setTo(subscriber.getUser());
			message.setFrom(new JID(configuration
					.getProperty("server.domain.channels")));
			outQueue.put(message.createCopy());
		}
	}
}
