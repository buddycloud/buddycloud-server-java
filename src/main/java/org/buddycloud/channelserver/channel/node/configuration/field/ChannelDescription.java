package org.buddycloud.channelserver.channel.node.configuration.field;

public class ChannelDescription extends Field
{
	public static final String FIELD_NAME    = "pubsub#description";
	public static final String DEFAULT_VALUE = "Channel description";

	public ChannelDescription()
	{
		name = FIELD_NAME;
	}
	
	public boolean isValid()
	{
		return true;
	}
}
