package org.buddycloud.channelserver.channel.node.configuration.field;

public class ChannelType extends Field
{
	public static final String FIELD_NAME    = "buddycloud#channel_type";
	public static final String DEFAULT_VALUE = AccessModel.models.OPEN.toString();
	
	public enum models { 
		PERSONAL("personal"), TOPIC("topic");
		String model = null;
		private models(String model) {
			this.model = model;
		}
		public String toString() {
	        return model;
	    }
	}

	public boolean valid()
	{
		return (getValue().equals(ChannelType.models.PERSONAL)
			|| getValue().equals(ChannelType.models.TOPIC)
		);
	}
}
