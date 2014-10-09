package org.buddycloud.channelserver.channel.node.configuration.field;

import org.xmpp.packet.JID;

public class Creator extends Field {
    public static final String FIELD_NAME = "pubsub#creator";
    public static final String DEFAULT_VALUE = "";

    public Creator() {
        name = FIELD_NAME;
    }

    public boolean isValid() {
        try {
            JID owner = new JID(getValue());
            owner.toBareJID();
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }
}
