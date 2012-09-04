package org.buddycloud.channelserver.channel;

import java.util.Map;

import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

public class ChannelManagerImpl implements ChannelManager {

	private final NodeStore nodeStore;
	
	/**
	 * Create an instance backed by a {@link NodeStore}.
	 * @param nodeStore the backing {@link NodeStore}.
	 */
	public ChannelManagerImpl(final NodeStore nodeStore) {
		this.nodeStore = nodeStore;
	}
	
	@Override
	public void createChannel(JID channelJID, JID channelOwner, Map<String, String> configuration) {
		// TODO Auto-generated method stub

	}

	@Override
	public void updateChannelConfiguration(JID channelJID,
			Map<String, String> configuration) {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean channelExists(JID channelJID) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setChannelSubscription(JID user, JID channelJID,
			Subscriptions subscription) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setChannelAffiliation(JID user, JID channelJID,
			Affiliations affiliation) {
		// TODO Auto-generated method stub

	}

}
