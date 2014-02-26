package org.buddycloud.channelserver.channel.node.configuration.field;

import java.util.Date;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.Conf;

public abstract class DateField extends Field
{
	public static final String DEFAULT_VALUE = "1955-11-05T01:21:00Z";
	
	public static Logger logger = Logger.getLogger(DateField.class);

	public DateField() {}

	abstract public boolean isValid();
}
