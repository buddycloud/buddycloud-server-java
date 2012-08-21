package org.buddycloud.channelserver.channel.node.configuration.field;

public class ChannelTitle extends Field
{
	public static final String FIELD_NAME    = "pubsub#title";
	public static final String DEFAULT_VALUE = "buddycloud channel title";

	public boolean isValid()
	{
		return true;
	}
}
