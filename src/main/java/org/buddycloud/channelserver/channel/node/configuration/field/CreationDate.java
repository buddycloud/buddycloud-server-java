package org.buddycloud.channelserver.channel.node.configuration.field;

import java.util.Date;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.Conf;

public class CreationDate extends DateField {
    public static final String FIELD_NAME = "pubsub#creation_date";
    public static final String DEFAULT_VALUE = "1955-11-05T01:21:00Z";

    public static final Logger LOGGER = Logger.getLogger(CreationDate.class);

    public CreationDate() {
        setValue(Conf.formatDate(new Date()));
        name = FIELD_NAME;
    }

    public boolean isValid() {
        // @todo improve this validation later
        try {
            Date parsed = Conf.parseDate(getValue());
            setValue(Conf.formatDate(parsed));
            return true;
        } catch (IllegalArgumentException e) {
            LOGGER.error(e);
            return false;
        }
    }
}
