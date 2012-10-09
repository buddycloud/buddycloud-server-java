package org.buddycloud.channelserver.pubsub.model.impl;

import nl.jqno.equalsverifier.EqualsVerifier;

import org.junit.Test;
import org.junit.Ignore;

public class NodeSubscriptionImplTest {
	@Test
	public void testEquals() {
		EqualsVerifier.forClass(NodeSubscriptionImpl.class).verify();
	}

}