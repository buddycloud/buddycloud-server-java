package org.buddycloud.channelserver.pubsub.model.impl;

import static org.junit.Assert.assertEquals;
import nl.jqno.equalsverifier.EqualsVerifier;

import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.junit.Ignore;
import org.xmpp.packet.JID;

public class NodeSubscriptionImplTest {
	@Test
	public void testChannelDomainListenerReturnsAsExpected() {
		NodeSubscriptionImpl subscription = new NodeSubscriptionImpl("node",
				new JID("user@domain.com"), new JID("channels.domain.com"),
				Subscriptions.subscribed);
	    assertEquals(new JID("channels.domain.com"), subscription.getListener());
	}

	@Test
	public void testListenerWithResourceHasThisRemovedOnRetrieval() {
		NodeSubscriptionImpl subscription = new NodeSubscriptionImpl("node",
				new JID("user@domain.com"), new JID("user@domain.com/resource"),
				Subscriptions.subscribed);
	    assertEquals(new JID("user@domain.com"), subscription.getListener());
	}
}