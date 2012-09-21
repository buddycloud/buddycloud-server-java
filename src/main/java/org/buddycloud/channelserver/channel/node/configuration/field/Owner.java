package org.buddycloud.channelserver.channel.node.configuration.field;

public class Owner extends Field
{
	public static final String FIELD_NAME    = "pubsub#owner";
	public static final String DEFAULT_VALUE = "";

	public Owner()
	{
		name = FIELD_NAME;
	}
	
	public boolean isValid()
	{
		return true;
	}
}
