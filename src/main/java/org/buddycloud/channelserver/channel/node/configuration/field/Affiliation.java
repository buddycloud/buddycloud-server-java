package org.buddycloud.channelserver.channel.node.configuration.field;

public class Affiliation extends Field
{
	public static final String FIELD_NAME    = "pubsub#default_affiliation";
	public static final String DEFAULT_VALUE = Affiliation.models.MEMBER.toString();
	
	public enum models { 
		FOLLOWER_AND_POST("publisher"), MEMBER("follower");
		String model = null;
		private models(String model) {
			this.model = model;
		}
		public String toString() {
	        return model;
	    }
	}

	public Affiliation()
	{
		name = FIELD_NAME;
	}
	
	public boolean isValid()
	{
		return (getValue().equals(Affiliation.models.FOLLOWER_AND_POST)
			|| getValue().equals(Affiliation.models.MEMBER)
		);
	}
}
