package org.buddycloud.channelserver.pubsub.affiliation;

public enum Affiliations {
	
		owner,
		publisher,
		publish_only {
		    public String toString() {
		        return "publish-only";
		    }
		},
		member,
		none,
		outcast;

		public static Affiliations createFromString(String asString) {
			
			if("owner".equals(asString)) {
				return owner;
			} else if ("publisher".equals(asString)) {
				return publisher;
			} else if ("publish-only".equals(asString)) {
				return publish_only;
			} else if ("member".equals(asString)) {
				return member;
			} else if ("none".equals(asString)) {
				return none;
			} else if ("outcast".equals(asString)) {
				return outcast;
			}
			return none;
		}
		
}
