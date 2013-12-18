package org.buddycloud.channelserver.pubsub.affiliation;

public enum Affiliations {

	owner, moderator, publisher, member, none, outcast;

	public static Affiliations createFromString(String asString) {

		if ("owner".equals(asString)) {
			return owner;
		} else if ("publisher".equals(asString)) {
			return publisher;
		} else if ("moderator".equals(asString)) {
			return moderator;
		} else if ("member".equals(asString)) {
			return member;
		} else if ("none".equals(asString)) {
			return none;
		} else if ("outcast".equals(asString)) {
			return outcast;
		}
		return none;
	}

	public static Affiliations createFromBuddycloudString(String bcString) {

		if ("producer".equals(bcString)) {
			return owner;
		} else if ("follower+post".equals(bcString)) {
			return publisher;
		} else if ("moderator".equals(bcString)) {
			return moderator;
		} else if ("follower".equals(bcString)) {
			return member;
		} else if ("none".equals(bcString)) {
			return none;
		} else if ("banned".equals(bcString)) {
			return outcast;
		}
		return none;
	}
    
	public boolean in(Affiliations... affiliations) {
		for (Affiliations a : affiliations) {
			if (a.equals(this)) return true;
		}
		return false;
	}

	public boolean canAuthorize() {
		return in(owner, moderator);
	}
}
