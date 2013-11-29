package org.buddycloud.channelserver.channel.node.configuration.field;

public class Factory
{	
	private boolean allowOwner = false;
	
	public void setAllowOwner(boolean allowOwner) {
		this.allowOwner = allowOwner;
	}
	
	public Field create(String type, String value)
    {
    	if ((null == type) || (null == value)) {
    		throw new ConfigurationFieldException();
    	}
    	if (type.equals("pubsub#owner") && (false == this.allowOwner)) {
    		throw new ConfigurationFieldException();
    	} else if (type.equals(ChannelTitle.FIELD_NAME)) {
    	    ChannelTitle field = new ChannelTitle();
    	    field.setValue(value);
    	    return field;
    	} else if (type.equals(ChannelDescription.FIELD_NAME)) {
    		ChannelDescription field = new ChannelDescription();
    		field.setValue(value);
    		return field;
    	} else if (type.equals(AccessModel.FIELD_NAME)) {
    		AccessModel field = new AccessModel();
    		field.setValue(value);
    		return field;
    	} else if (type.equals(Affiliation.FIELD_NAME)) {
    		Affiliation field = new Affiliation();
    		field.setValue(value);
    		return field;
    	} else if (type.equals(CreationDate.FIELD_NAME)) {
    		CreationDate field = new CreationDate();
    		field.setValue(value);
    		return field;
    	} else if (type.equals(ChannelType.FIELD_NAME)) {
    		ChannelType field = new ChannelType();
    	    field.setValue(value);
    	    return field;
    	} else {
    		Generic field = new Generic();
    	    field.setName(type);
    	    field.setValue(value);
    	    return field;
    	}
    }
}