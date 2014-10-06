package org.buddycloud.channelserver.channel;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.xmpp.packet.JID;

public class ChannelNodeRefTest {

    @Test
    public void testToString() {
        JID jid = new JID("user@thing.com");
        String type = "posts";

        String expected = "/user/user@thing.com/posts";

        ChannelNodeRef ref = new ChannelNodeRef(jid, type);

        assertEquals(expected, ref.toString());
    }

    @Test
    public void testFromNodeId() {
        String nodeId = "/user/user@thing.com/posts";

        JID jid = new JID("user@thing.com");
        String type = "posts";

        ChannelNodeRef ref = ChannelNodeRef.fromNodeId(nodeId);

        assertEquals("Unexpected JID", jid, ref.getJID());
        assertEquals("Unexpected type", type, ref.getType());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testFromNodeIdWithInvalidId() {
        String nodeId = "some random invalid id";

        ChannelNodeRef.fromNodeId(nodeId);
    }

}
