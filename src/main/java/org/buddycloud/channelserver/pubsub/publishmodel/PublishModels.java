package org.buddycloud.channelserver.pubsub.publishmodel;

public enum PublishModels {
	
    publishers,
    subscribers,
    open;

	public static PublishModels createFromString(String asString) {
		
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
