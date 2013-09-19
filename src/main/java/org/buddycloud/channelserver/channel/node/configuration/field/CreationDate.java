package org.buddycloud.channelserver.channel.node.configuration.field;

import java.util.Date;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import org.apache.log4j.Logger;

public class CreationDate extends Field
{
	public static final String FIELD_NAME    = "pubsub#creation_date";
	public static final String DEFAULT_VALUE = "1955-11-05T01:21:00Z";
	
	public static Logger logger = Logger.getLogger(CreationDate.class);

	public static SimpleDateFormat ISO8601FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.S'Z'");
	
	public CreationDate()
	{
		setValue(ISO8601FORMAT.format(new Date()));
		name = FIELD_NAME;
	}

	public boolean isValid()
	{
		// @todo improve this validation later
		try {
			Date parsed = ISO8601FORMAT.parse(getValue());
			setValue(ISO8601FORMAT.format(parsed));
			return true;
		} catch (ParseException e) {
			e.printStackTrace();
			logger.error(e);
			return false;
		}
	}
}
