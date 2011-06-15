package org.buddycloud.channels.pubsub;

import java.util.HashMap;
import java.util.Map;


public final class Subscription {

	public static final String KEY_SUBSCRIPTION = "subscription";
	public static final String KEY_AFFILIATION  = "affiliation";
	public static final String KEY_EXTERNAL_CHANNEL_SERVER = "channel-server";
	
	
	private Map<String, String> params = new HashMap<String, String>();
	
	public Subscription(org.buddycloud.channels.pubsub.subscription.Type sub, 
						org.buddycloud.channels.pubsub.affiliation.Type aff,
						String remoteChannelServer) {
		
		this.params.put(KEY_SUBSCRIPTION, sub.toString());
		this.params.put(KEY_AFFILIATION, aff.toString());
		
		if(remoteChannelServer != null) {
			this.params.put(KEY_EXTERNAL_CHANNEL_SERVER, remoteChannelServer);
		}
	}
	
	public Map<String, String> getAsMap() {
		return this.params;
	}
	
	public String getSubscription() {
		return this.params.get("subscription");
	}
	
}
