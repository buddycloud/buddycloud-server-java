package org.buddycloud.channelserver.pubsub.model.impl;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

public class NodeSubscriptionImpl implements NodeSubscription {

	private final Subscriptions subscription;
	private final JID user;
	private final JID listener; // If different from user
	private final String nodeId;

	public NodeSubscriptionImpl(final String nodeId, final JID user,
			final Subscriptions subscription) {
		this(nodeId, user, user, subscription);
	}

	public NodeSubscriptionImpl(final String nodeId, final JID user,
			final JID listener, final Subscriptions subscription) {
		this.nodeId = nodeId;
		if (user.getResource() == null) {
			this.user = user;
		} else {
			this.user = new JID(user.toBareJID());
		}
		this.listener = listener;
		this.subscription = subscription;
	}

	@Override
	public Subscriptions getSubscription() {
		return subscription;
	}

	@Override
	public JID getUser() {
		return user;
	}

	@Override
	public JID getListener() {
		return listener;
	}

	@Override
	public String getNodeId() {
		return nodeId;
	}

	@Override
	public String toString() {
		return "NodeSubscriptionImpl [subscription=" + subscription + ", user="
				+ user + ", listener=" + listener + ", nodeId=" + nodeId + "]";
	}
}
