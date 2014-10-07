package org.buddycloud.channelserver.channel.node.configuration.field;

public class NodeDescription extends Field {
    public static final String FIELD_NAME = "pubsub#description";
    public static final String DEFAULT_VALUE = "Channel description";
    public static final int MAX_DESCRIPTION_LENGTH = 1024;

    public NodeDescription() {
        name = FIELD_NAME;
    }


    public String getValue() {
        if (this.value.length() > MAX_DESCRIPTION_LENGTH) {
            this.value = this.value.substring(0, MAX_DESCRIPTION_LENGTH);
        }
        return this.value;
    }

    public boolean isValid() {
        return true;
    }
}
