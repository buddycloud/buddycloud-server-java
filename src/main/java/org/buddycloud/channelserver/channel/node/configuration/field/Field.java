package org.buddycloud.channelserver.channel.node.configuration.field;

public abstract class Field<E>
{
    public static String FIELD_NAME    = "FIELD";
    public static String DEFAULT_VALUE = "DEFAULT_VALUE";

    protected String value;
    protected String name;
    
    public void setValue(String value)
    {
    	this.value = value;
    }
    
    public String getName()
    {
    	if ((null == name) || name.equals("")) {
    		throw new ConfigurationFieldException("No field name set");
    	}
    	return name;
    }
    
    public String getValue()
    {
    	return this.value;
    }

    public abstract boolean isValid();
}
