package org.buddycloud.channelserver.channel.node.configuration.field;

import org.apache.log4j.Logger;

public abstract class DateField extends Field {
    public static final String DEFAULT_VALUE = "1955-11-05T01:21:00Z";

    public static Logger logger = Logger.getLogger(DateField.class);

    public DateField() {}

    public abstract boolean isValid();
}
