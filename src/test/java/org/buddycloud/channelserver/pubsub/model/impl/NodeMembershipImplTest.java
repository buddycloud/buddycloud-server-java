package org.buddycloud.channelserver.pubsub.model.impl;

import static org.junit.Assert.assertEquals;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.packet.JID;

public class NodeMembershipImplTest {
	
	@Test
	public void nodeDomainListenerReturnsAsExpected() {
		NodeMembershipImpl membership = new NodeMembershipImpl("node",
				new JID("user@domain.com"), new JID("channels.domain.com"),
				Subscriptions.subscribed, Affiliations.none);
	    assertEquals(new JID("channels.domain.com"), membership.getListener());
	}

	@Test
	public void listenerWithResourceHasThisRemovedOnRetrieval() {
		NodeMembershipImpl membership = new NodeMembershipImpl("node",
				new JID("user@domain.com"), new JID("user@domain.com/resource"),
				Subscriptions.subscribed, Affiliations.none);
	    assertEquals(new JID("user@domain.com"), membership.getListener());
	}
	
	@Test
	public void subscriptionReturnsAsExpected() {
		NodeMembershipImpl membership = new NodeMembershipImpl("node",
				new JID("user@domain.com"), new JID("channels.domain.com"),
				Subscriptions.subscribed, Affiliations.none);
	    assertEquals(Subscriptions.subscribed, membership.getSubscription());
	}
	
	@Test
	public void affiliationReturnsAsExpected() {
		NodeMembershipImpl membership = new NodeMembershipImpl("node",
				new JID("user@domain.com"), new JID("channels.domain.com"),
				Subscriptions.subscribed, Affiliations.none);
	    assertEquals(Affiliations.none, membership.getAffiliation());
	}
}