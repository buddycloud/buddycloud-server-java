package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class NotificationSendingMock extends AbstractMessageProcessor {

	public NotificationSendingMock(ChannelManager channelManager,
			Properties configuration, BlockingQueue<Packet> outQueue) {
		super(channelManager, configuration, outQueue);
	}

	@Override
	public void process(Message packet) throws Exception {
		message = packet;
		
		JID jid = null;
		if (null != message.getElement().attributeValue("jid")) {
			jid = new JID(message.getElement().attributeValue("jid"));
		}
		sendLocalNotifications(Integer.parseInt(message.getElement().attributeValue("scheme")), jid);
	}

}
