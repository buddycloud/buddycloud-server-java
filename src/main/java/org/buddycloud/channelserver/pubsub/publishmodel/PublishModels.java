package org.buddycloud.channelserver.pubsub.publishmodel;

public enum PublishModels {
	
    owner,
    moderator,
    publisher,
    member,
    outcast;
	
	public static PublishModels createFromString(String asString) {
		
		if ("owner".equals(asString)) {
			return owner;
		} else if ("moderator".equals(asString)) {
			return moderator;
		} else if ("publisher".equals(asString)) {
			return publisher;
		} else if ("outcast".equals(asString)) {
			return outcast;
		}
		return member;
	}
		
}
