package org.buddycloud.channelserver.utils;

public enum NotificationScheme {

	SCHEME_VALID_SUBSCRIBERS, SCHEME_OWNER_MODERATOR;
	
	public static final String UNSUPPORTED_SCHEME = "Unsupported notification scheme type";
	
	public static NotificationScheme createFromString(String asString) {
		if ("1".equals(asString)) {
			return SCHEME_VALID_SUBSCRIBERS;
		} else if ("2".equals(asString)) {
			return SCHEME_OWNER_MODERATOR;
		}
		return SCHEME_VALID_SUBSCRIBERS;
	}
}