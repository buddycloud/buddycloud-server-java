package org.buddycloud.channelserver.pubsub.model.impl;

import java.util.Date;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.Result;

public class NodeMembershipImpl implements NodeMembership {

	private final Subscriptions subscription;
	private final JID user;
	private JID listener; // If different from user
	private final String nodeId;
	private Date lastUpdated;
	private Affiliations affiliation;


	public NodeMembershipImpl(final String nodeId, final JID user,
			final Subscriptions subscription, final Affiliations affiliation) {
		this(nodeId, user, user, subscription, affiliation, new Date());
	}
	
	public NodeMembershipImpl(final String nodeId, final JID user,
			final Subscriptions subscription, Affiliations affiliation, Date lastUpdated) {
		this(nodeId, user, user, subscription, affiliation, lastUpdated);
	}

	public NodeMembershipImpl(final String nodeId, final JID user,
			JID listener, final Subscriptions subscription, final Affiliations affiliation) {
		this(nodeId, user, listener, subscription, affiliation, new Date());
	}
	
	public NodeMembershipImpl(final String nodeId, final JID user,
			JID listener, final Subscriptions subscription, final Affiliations affiliation, Date lastUpdated) {
		this.nodeId = nodeId;
		if (user.getResource() == null) {
			this.user = user;
		} else {
			this.user = new JID(user.toBareJID());
		}
		this.lastUpdated = lastUpdated;
		setListener(listener);
		this.subscription = subscription;
		this.affiliation = affiliation;
	}

	private void setListener(JID listener) {
		if (null == listener) {
			listener = this.user;
			return;
		}
		if (null == listener.getNode()) {
			this.listener = new JID(listener.getDomain());
			return;
		}
		this.listener = new JID(listener.toBareJID());
	}

	@Override
	public Subscriptions getSubscription() {
		return subscription;
	}
	
	@Override
	public Affiliations getAffiliation() {
		return affiliation;
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
		result = prime * result
				+ ((affiliation == null) ? 0 : affiliation.hashCode());
		result = prime * result + ((user == null) ? 0 : user.hashCode());
		return result;
	}

	@Override
	public final boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof NodeMembershipImpl))
			return false;
		NodeMembershipImpl other = (NodeMembershipImpl) obj;
		if (listener == null) {
			if (other.listener != null)
				return false;
		} else if (!listener.equals(other.listener))
			return false;
		if (nodeId == null) {
			if (other.nodeId != null)
				return false;
		} else if (!nodeId.equals(other.nodeId)) {
			return false;
		}
		if (subscription != other.subscription) {
			return false;
		}
		if (affiliation != other.affiliation) {
			return false;
		}
		if (user == null) {
			if (other.user != null) {
				return false;
			}
		} else if (!user.equals(other.user)) {
			return false;
		}
		return true;
	}
	
	@Override
	public String toString() {
		return "NodeMembershipImpl [subscription=" + subscription +
				", affiliation=" + affiliation + ",user=" +
				user + ", listener=" + listener + ", nodeId=" + nodeId + "]";
	}

	@Override
	public String getUID() {
		return toString();
	}

	@Override
	public Date getLastUpdated() {
		return lastUpdated;
	}
}