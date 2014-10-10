package org.buddycloud.channelserver.utils.node;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class NodeAclRefuseReasonTest extends TestCase {

    Type type = PacketError.Type.cancel;
    Condition condition = PacketError.Condition.feature_not_implemented;

    NodeAclRefuseReason reason;

    @Before
    public void setUp() {
        reason = new NodeAclRefuseReason(type, condition, null);
    }

    @Test
    public void testCallingGetTypeReturnsExpectedErrorType() {
        assertEquals(type.toString(), reason.getType().toString());
    }

    @Test
    public void testCallingGetConditionReturnsExpectedErrorCondition() {
        assertEquals(condition.toString(), reason.getCondition().toString());
    }

    @Test
    public void testCallingGetAdditionalErrorElementWithoutSettingReturnsNull() {
        assertNull(reason.getAdditionalErrorElement());
    }

    @Test
    public void testPassingAnAdditionErrorElementRetunsAsExpected() {
        reason = new NodeAclRefuseReason(type, condition, "nodeid-required");
        assertEquals("nodeid-required", reason.getAdditionalErrorElement());
    }
}
