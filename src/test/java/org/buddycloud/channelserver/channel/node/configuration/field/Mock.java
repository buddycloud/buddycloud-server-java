package org.buddycloud.channelserver.channel.node.configuration.field;

public class Mock extends Field
{
    public static final String FIELD_NAME    = "MOCK_FIELD";
    public static final String DEFAULT_VALUE = "MOCK_DEFAULT_VALUE";
    
    public Mock()
	{
		name = FIELD_NAME;
	}
    
    public Mock(String name)
    {
    	this.name = name;
    }
	
	public boolean isValid()
	{
		return false;
	}
}