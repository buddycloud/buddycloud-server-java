package org.buddycloud.channelserver.pubsub.accessmodel;

public enum AccessModels {

	authorize, open, presence, roster, whitelist, local;

	public static AccessModels createFromString(String asString) {

		if ("authorize".equals(asString)) {
			return authorize;
		} else if ("open".equals(asString)) {
			return open;
		} else if ("presence".equals(asString)) {
			return presence;
		} else if ("roster".equals(asString)) {
			return roster;
		} else if ("whitelist".equals(asString)) {
			return whitelist;
		} else if ("local".equals(asString)) {
			return local;
		}
		return authorize;
	}

}
