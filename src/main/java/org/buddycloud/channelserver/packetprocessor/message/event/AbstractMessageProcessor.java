package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

abstract public class AbstractMessageProcessor implements PacketProcessor<Message> {

	private static final String UNSUPPORTED_SCHEME = "Unsupported notification scheme type";
	
	protected Message message;
	protected String node;
	protected ChannelManager channelManager;
	protected Properties configuration;
	protected BlockingQueue<Packet> outQueue;
	
	public final int SCHEME_VALID_SUBSCRIBERS = 1;
	public final int SCHEME_VALID_OWNER_MODERATOR = 2;
	
	
	public AbstractMessageProcessor(ChannelManager channelManager, Properties configuration, BlockingQueue<Packet> outQueue) {
		this.channelManager = channelManager;
		setConfiguration(configuration);
		this.outQueue = outQueue;
	}
	
	public void setConfiguration(Properties configuration) {
		this.configuration = configuration;
	}
	
	abstract public void process(Message packet) throws Exception;
	
	void sendLocalNotifications(int scheme) throws Exception {
	    sendLocalNotifications(scheme, null);	
	}
	
	void sendLocalNotifications(int scheme, JID user) throws Exception {
		ResultSet<NodeMembership> members = channelManager
				.getNodeMemberships(node);
		message.setFrom(new JID(configuration
				.getProperty(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN)));
		
		for (NodeMembership member : members) {
			if (false == channelManager.isLocalJID(member.getUser())) continue;
			
			switch (scheme) {
			    case SCHEME_VALID_SUBSCRIBERS: 
				    if (false == userIsValidSubscriber(member)) {
				    	continue;
				    }
				    break;
			    case SCHEME_VALID_OWNER_MODERATOR:
			    	if (false == userIsOwnerOrModerator(member)) {
			    		continue;
			    	}
			    	break;
			    default:
			    	throw new UnsupportedOperationException(UNSUPPORTED_SCHEME);
			}

			
			message.setTo(member.getUser());
			outQueue.put(message.createCopy());
		}
		
		if (null != user) {
			message.setTo(user.toBareJID());
			outQueue.put(message.createCopy());
		}
	}

	private boolean userIsOwnerOrModerator(NodeMembership member) {
		if (false == Subscriptions.subscribed.equals(member.getSubscription())) {
			return false;
		}
		if (false == member.getAffiliation().canAuthorize()) {
			return false;
		}
		return true;
	}

	private boolean userIsValidSubscriber(NodeMembership member) {
		if (false == Subscriptions.subscribed.equals(member.getSubscription())) {
			return false;
		}
		if (Affiliations.outcast.equals(member.getAffiliation())) {
			return false;
		}
		return true;
	}
}