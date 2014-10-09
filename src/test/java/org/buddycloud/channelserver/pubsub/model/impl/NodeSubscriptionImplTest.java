package org.buddycloud.channelserver.pubsub.model.impl;

import static org.junit.Assert.assertEquals;

import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.packet.JID;

public class NodeSubscriptionImplTest {
    @Test
    public void testChannelDomainListenerReturnsAsExpected() {
        NodeSubscriptionImpl subscription =
                new NodeSubscriptionImpl("node", new JID("user@domain.com"), new JID("channels.domain.com"), Subscriptions.subscribed, null);
        assertEquals(new JID("channels.domain.com"), subscription.getListener());
    }

    @Test
    public void testListenerWithResourceHasThisRemovedOnRetrieval() {
        NodeSubscriptionImpl subscription =
                new NodeSubscriptionImpl("node", new JID("user@domain.com"), new JID("user@domain.com/resource"), Subscriptions.subscribed, null);
        assertEquals(new JID("user@domain.com"), subscription.getListener());
    }
}
