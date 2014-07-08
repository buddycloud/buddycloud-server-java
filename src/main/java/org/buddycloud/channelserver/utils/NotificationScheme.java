package org.buddycloud.channelserver.utils;

public enum NotificationScheme {

	validSubscribers, ownerOrModerator;
	
	public static final String UNSUPPORTED_SCHEME = "Unsupported notification scheme type";
	
	public static NotificationScheme createFromString(String asString) {
		if ("1".equals(asString)) {
			return validSubscribers;
		} else if ("2".equals(asString)) {
			return ownerOrModerator;
		}
		return validSubscribers;
	}
}