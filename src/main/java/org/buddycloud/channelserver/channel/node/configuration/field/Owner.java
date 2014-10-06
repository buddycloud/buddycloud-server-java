package org.buddycloud.channelserver.channel.node.configuration.field;

import org.xmpp.packet.JID;

public class Owner extends Field {
    public static final String FIELD_NAME = "buddycloud#owner";
    public static final String DEFAULT_VALUE = "";

    public Owner() {
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
