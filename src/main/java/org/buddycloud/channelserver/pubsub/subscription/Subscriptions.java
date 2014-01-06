package org.buddycloud.channelserver.pubsub.subscription;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;

public enum Subscriptions {
	none, invited, pending, unconfigured, subscribed;

	public static Subscriptions createFromString(String asString) {
		if ("none".equals(asString)) {
			return none;
		} else if ("invited".equals(asString)) {
			return invited;
		} else if ("pending".equals(asString)) {
			return pending;
		} else if ("unconfigured".equals(asString)) {
			return unconfigured;
		} else if ("subscribed".equals(asString)) {
			return subscribed;
		}
		return none;
	}

	public boolean in(Subscriptions... subscriptions) {
		for (Subscriptions subscription : subscriptions) {
			if (subscription.equals(this)) {
				return true;
			}
		}
		return false;
	}
}