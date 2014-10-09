package org.buddycloud.channelserver.channel.node.configuration.field;

import java.util.Date;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.Conf;

public class LastUpdatedDate extends DateField {
    public static final String FIELD_NAME = "buddycloud#last_updated";

    public static Logger logger = Logger.getLogger(LastUpdatedDate.class);

    public LastUpdatedDate() {
        super();
        this.name = FIELD_NAME;
    }

    public void setValue(String value) {
        this.value = Conf.formatDate(new Date());
    }

    public boolean isValid() {
        return true;
    }
}
