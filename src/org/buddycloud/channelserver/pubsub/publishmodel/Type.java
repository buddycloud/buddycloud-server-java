package org.buddycloud.channelserver.pubsub.publishmodel;

public enum Type {
	
    publishers,
    subscribers,
    open;

	public static Type createFromString(String asString) {
		
		if("publishers".equals(asString)) {
			return publishers;
		} else if ("subscribers".equals(asString)) {
			return subscribers;
		} else if ("open".equals(asString)) {
			return open;
		}
		return publishers;
	}
		
}
