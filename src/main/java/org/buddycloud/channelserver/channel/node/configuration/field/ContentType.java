package org.buddycloud.channelserver.channel.node.configuration.field;

import org.buddycloud.channelserver.utils.node.item.payload.Atom;

public class ContentType extends Field {
    public static final String FIELD_NAME = "pubsub#type";
    public static final String DEFAULT_VALUE = Atom.NS;

    public ContentType() {
        name = FIELD_NAME;
    }

    public boolean isValid() {
        return true;
    }
}
