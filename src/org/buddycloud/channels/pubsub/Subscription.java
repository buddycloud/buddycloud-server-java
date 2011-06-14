package org.buddycloud.channels.pubsub;

import java.util.HashMap;
import java.util.Map;


public final class Subscription {

	private Map<String, String> params = new HashMap<String, String>();
	
	public Subscription(org.buddycloud.channels.pubsub.subscription.Type sub, 
						org.buddycloud.channels.pubsub.affiliation.Type aff,
						String remoteChannelServer) {
		
		this.params.put("subscription", sub.toString());
		this.params.put("affiliation", aff.toString());
		
		if(remoteChannelServer != null) {
			this.params.put("channel-server", remoteChannelServer);
		}
	}
	
	public Map<String, String> getAsMap() {
		return this.params;
	}
	
	public String getSubscription() {
		return this.params.get("subscription");
	}
	
}
