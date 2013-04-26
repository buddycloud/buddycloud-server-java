package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.dom4j.Element;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
 
public class DeleteProcessor extends AbstractMessageProcessor  {
 
	private static final Logger logger = Logger
			.getLogger(DeleteProcessor.class);

	public DeleteProcessor(BlockingQueue<Packet> outQueue,
			Properties configuration, ChannelManager channelManager) {
		super(channelManager, configuration, outQueue);
	}

	@Override
	public void process(Message packet) throws Exception {
		message = packet;

		deleteNode();

		if (false == channelManager.isLocalNode(node)) {
			sendLocalNotifications();
		}
	}

	private void deleteNode() throws NodeStoreException {
		Element deleteElement = message.getElement().element("event")
				.element("delete");
		if (null == deleteElement) {
			return;
		}
		node = deleteElement.attributeValue("node");
		if (true == channelManager.isLocalNode(node)) {
			return;
		}
        channelManager.deleteNode(node);
	}
}