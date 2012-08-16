package org.buddycloud.channelserver.channel.node.configuration.field;

import java.util.Date;
import java.text.SimpleDateFormat;

public class CreationDate extends Field
{
	public static final String FIELD_NAME    = "pubsub#creation_date";
	public static final String DEFAULT_VALUE = "1955-11-05T01:21:00Z";

	public static SimpleDateFormat ISO8601FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
	
	public CreationDate()
	{
		setValue(ISO8601FORMAT.format(new Date()));
	}

	public boolean valid()
	{
		// @todo improve this validation later
		return getValue().matches("/^[0-9]{4}-[0-9}{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$/");
	}
}
