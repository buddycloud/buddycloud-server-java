package org.buddycloud.channelserver.channel;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.xmpp.packet.JID;

public class ChannelNodeRef {
    private static Pattern pattern = Pattern.compile("^/user/([^/]+)/?(.*)$");

    private JID jid;
    private String type;

    public ChannelNodeRef(final JID jid, final String type) {
        this.jid = jid;
        this.type = type;
    }

    /**
     * Returns the jid portion of the node id.
     * 
     * @return
     */
    public JID getJID() {
        return jid;
    }

    /**
     * Returns the type of the node (e.g. "posts", "subscriptions", etc).
     * 
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * Converts the node ref into a node id string.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("/user/");
        sb.append(jid.toBareJID());

        if (type != null) {
            sb.append("/");
            sb.append(type);
        }

        return sb.toString();
    }

    /**
     * Parses a node id into a channel node ref.
     * 
     * @param nodeId the node id string.
     * @return the channel ref.
     * @throws IllegalArgumentException if the node id does not represent a valid channels protocol
     *         node.
     */
    public static ChannelNodeRef fromNodeId(final String nodeId) {
        Matcher m = pattern.matcher(nodeId);

        if (m.matches()) {
            return new ChannelNodeRef(new JID(m.group(1)), m.group(2));
        }

        throw new IllegalArgumentException(nodeId + " does represent a valid channel node id");
    }
}
