package org.buddycloud.channelserver.utils.matcher;

import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.mockito.internal.matchers.Equals;
import org.xmpp.packet.JID;

public class NodeSubscriptionMatcher extends Equals {

	private NodeSubscriptionImpl wanted;
	private NodeSubscriptionImpl received;

	public NodeSubscriptionMatcher(Object wanted) {
		super(wanted);
	}

	public boolean matches(Object subscripition) {
		wanted = (NodeSubscriptionImpl) getWanted();
		received = (NodeSubscriptionImpl) subscripition;

		if ((false == subscriptionMatch()) || (false == subscriberMatch())
				|| (false == inviterMatch()) || (false == nodeMatch())
				|| (false == lastUpdatedMatch())) {
			return false;
		}
		return true;
	}

	private boolean lastUpdatedMatch() {
		if (null == wanted.getLastUpdated()) return true;
		return wanted.getLastUpdated().equals(received.getLastUpdated());
	}

	private boolean nodeMatch() {
		return (wanted.getNodeId().equals(received.getNodeId()));
	}

	private boolean inviterMatch() {
		if (null == wanted.getInviter())
			return true;
		return wanted.getInviter().toBareJID()
				.equals(received.getInviter().toBareJID());
	}

	private boolean subscriberMatch() {
		return wanted.getUser().toBareJID()
				.equals(received.getUser().toBareJID());
	}

	private boolean subscriptionMatch() {
		return wanted.getSubscription().equals(received.getSubscription());
	}
}
