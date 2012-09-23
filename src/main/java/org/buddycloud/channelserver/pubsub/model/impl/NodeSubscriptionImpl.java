package org.buddycloud.channelserver.pubsub.model.impl;

import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

public class NodeSubscriptionImpl implements NodeSubscription {

	private final Subscriptions subscription;
	private final JID user;
	private final JID listener;	// If different from user
	private final String nodeId;
	
	public NodeSubscriptionImpl(final String nodeId, final JID user, final Subscriptions subscription) {
		this(nodeId, user, user, subscription);
	}
	
	public NodeSubscriptionImpl(final String nodeId, final JID user, final JID listener, final Subscriptions subscription) {
		this.nodeId = nodeId;
		if(user.getResource() == null) {
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
	public final int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((listener == null) ? 0 : listener.hashCode());
		result = prime * result + ((nodeId == null) ? 0 : nodeId.hashCode());
		result = prime * result
				+ ((subscription == null) ? 0 : subscription.hashCode());
		result = prime * result + ((user == null) ? 0 : user.hashCode());
		return result;
	}

	@Override
	public final boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof NodeSubscriptionImpl))
			return false;
		NodeSubscriptionImpl other = (NodeSubscriptionImpl) obj;
		if (listener == null) {
			if (other.listener != null)
				return false;
		} else if (!listener.equals(other.listener))
			return false;
		if (nodeId == null) {
			if (other.nodeId != null)
				return false;
		} else if (!nodeId.equals(other.nodeId))
			return false;
		if (subscription != other.subscription)
			return false;
		if (user == null) {
			if (other.user != null)
				return false;
		} else if (!user.equals(other.user))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "NodeSubscriptionImpl [subscription=" + subscription + ", user="
				+ user + ", listener=" + listener + ", nodeId=" + nodeId + "]";
	}
}
