package org.buddycloud.channelserver.channel.node.configuration.field;

public class ChannelTitle extends Field
{
	public static final String FIELD_NAME    = "pubsub#title";
	public static final String DEFAULT_VALUE = "buddycloud channel title";
	public static final int MAX_TITLE_LENGTH = 128;

	public ChannelTitle()
	{
		name = FIELD_NAME;
	}
	
    public String getValue()
    {
    	if (this.value.length() > MAX_TITLE_LENGTH) {
    		this.value = this.value.substring(0, MAX_TITLE_LENGTH);
    	}
    	return this.value;
    }
	
	public boolean isValid()
	{
		return true;
	}
}
