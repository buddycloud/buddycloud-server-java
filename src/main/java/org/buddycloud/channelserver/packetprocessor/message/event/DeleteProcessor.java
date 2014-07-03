package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.utils.NotificationScheme;
import org.dom4j.Element;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
 
public class DeleteProcessor extends AbstractMessageProcessor  {
 
	public DeleteProcessor(BlockingQueue<Packet> outQueue,
			Properties configuration, ChannelManager channelManager) {
		super(channelManager, configuration, outQueue);
	}

	@Override
	public void process(Message packet) throws Exception {
		message = packet;
		deleteNode();
		if (!channelManager.isLocalNode(node)) {
			sendLocalNotifications(NotificationScheme.validSubscribers);
		}
	}

	private void deleteNode() throws NodeStoreException {
		Element deleteElement = message.getElement().element("event")
				.element("delete");
		if (deleteElement == null) {
			return;
		}
		node = deleteElement.attributeValue("node");
		if (channelManager.isLocalNode(node)) {
			return;
		}
        channelManager.deleteNode(node);
	}
}