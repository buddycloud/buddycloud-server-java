package org.buddycloud.channelserver.pubsub.model.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.packet.JID;

public class NodeSubscriptionImplTest {

    private NodeSubscriptionImpl channels = new NodeSubscriptionImpl("node", new JID("user@domain.com"), new JID("channels.domain.com"),
            Subscriptions.subscribed, null);

    private NodeSubscriptionImpl userAtDomain = new NodeSubscriptionImpl("node", new JID("user@domain.com"), new JID("user@domain.com/resource"),
            Subscriptions.subscribed, null);

    private NodeSubscriptionImpl differentUserAtDomain = new NodeSubscriptionImpl("node", new JID("differentuser@domain.com"), new JID(
            "user@domain.com/resource"), Subscriptions.subscribed, null);

    @Test
    public void testChannelDomainListenerReturnsAsExpected() {
        assertEquals(new JID("channels.domain.com"), channels.getListener());
    }

    @Test
    public void testListenerWithResourceHasThisRemovedOnRetrieval() {

        assertEquals(new JID("user@domain.com"), userAtDomain.getListener());
    }

    @Test
    public void testEquality() {

        assertTrue(channels.equals(channels));

        assertFalse(channels.equals(userAtDomain));

        assertFalse(userAtDomain.equals(differentUserAtDomain));

    }
}
