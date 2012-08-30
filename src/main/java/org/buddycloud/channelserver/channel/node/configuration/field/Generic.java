package org.buddycloud.channelserver.channel.node.configuration.field;

public class Generic extends Field
{
	public static String FIELD_NAME          = "";
	public static final String DEFAULT_VALUE = "";
	
    public void setName(String name)
    {
    	this.name = name;
    }
    
	public boolean isValid()
	{
		return true;
	}
}
