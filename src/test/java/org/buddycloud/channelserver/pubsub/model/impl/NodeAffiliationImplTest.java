package org.buddycloud.channelserver.pubsub.model.impl;

import nl.jqno.equalsverifier.EqualsVerifier;

import org.junit.Test;

public class NodeAffiliationImplTest {

    @Test
    public void testEquals() {
        EqualsVerifier.forClass(NodeAffiliationImpl.class).verify();
    }

}
