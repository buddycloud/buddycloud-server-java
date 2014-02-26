package org.buddycloud.channelserver.channel.node.configuration.field;

import java.util.Date;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.Conf;

public class LastUpdatedDate extends DateField
{
	public static final String FIELD_NAME    = "buddycloud#last_updated";
	
	public static Logger logger = Logger.getLogger(LastUpdatedDate.class);

	public LastUpdatedDate()
	{
		setValue(Conf.formatDate(new Date()));
		name = FIELD_NAME;
	}

	public boolean isValid()
	{
		return true;
	}
}
