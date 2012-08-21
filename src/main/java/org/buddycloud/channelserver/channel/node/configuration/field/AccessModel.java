package org.buddycloud.channelserver.channel.node.configuration.field;

public class AccessModel extends Field
{
	public static final String FIELD_NAME    = "pubsub#access_model";
	public static final String DEFAULT_VALUE = AccessModel.models.OPEN.toString();
	
	public enum models { 
		OPEN("open"), WHITELIST("whitelist");
		String model = null;
		private models(String model) {
			this.model = model;
		}
		public String toString() {
	        return model;
	    }
	}

	public boolean isValid()
	{
		return (getValue().equals(AccessModel.models.OPEN)
			|| getValue().equals(AccessModel.models.WHITELIST)
		);
	}
}
