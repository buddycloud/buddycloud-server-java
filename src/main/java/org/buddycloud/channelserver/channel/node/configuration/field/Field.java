package org.buddycloud.channelserver.channel.node.configuration.field;

public abstract class Field
{
    public static String FIELD_NAME    = "FIELD";
    public static String DEFAULT_VALUE = "";

    protected String value;

    public void setValue(String value)
    {
    	this.value = value;
    }
    
    public String getName()
    {
    	return FIELD_NAME;
    }
    
    public String getValue()
    {
    	return this.value;
    }

    public abstract boolean isValid();
}
