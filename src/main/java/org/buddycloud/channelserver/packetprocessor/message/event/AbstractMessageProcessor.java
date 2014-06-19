package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

abstract public class AbstractMessageProcessor implements PacketProcessor<Message> {

	protected Message message;
	protected String node;
	protected ChannelManager channelManager;
	protected Properties configuration;
	protected BlockingQueue<Packet> outQueue;
	
	public AbstractMessageProcessor(ChannelManager channelManager, Properties configuration, BlockingQueue<Packet> outQueue) {
		this.channelManager = channelManager;
		setConfiguration(configuration);
		this.outQueue = outQueue;
	}
	
	public void setConfiguration(Properties configuration) {
		this.configuration = configuration;
	}
	
	abstract public void process(Message packet) throws Exception;
	
	void sendLocalNotifications() throws Exception {
		ResultSet<NodeMembership> members = channelManager
				.getNodeMemberships(node);
		for (NodeMembership member : members) {
			if (false == channelManager.isLocalJID(member.getUser())) continue;
			message.setTo(member.getUser());
			message.setFrom(new JID(configuration
					.getProperty("server.domain.channels")));
			outQueue.put(message.createCopy());
		}
	}
}