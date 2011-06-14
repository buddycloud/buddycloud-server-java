package org.buddycloud.channels.pubsub.subscription;

public enum Type {
	
	/**
	 * The node MUST NOT send event notifications or payloads to the Entity.
	 */
	none,
	
	/**
	 * An entity has subscribed but its subscription options have not yet been 
	 * configured. The node MAY send event notifications or payloads to the entity 
	 * while it is in this state. The service MAY timeout unconfigured subscriptions.
	 */
	unconfigured,
	
	/**
	 * An entity is subscribed to a node. The node MUST send all event notifications (and, if 
	 * configured, payloads) to the entity while it is in this state (subject to subscriber 
	 * configuration and content filtering).
	 */
	subscribed,
	
	/**
	 * An entity has requested to subscribe to a node and the request has not yet been 
	 * approved by a node owner. The node MUST NOT send event notifications or payloads 
	 * to the entity while it is in this state.
	 */
	pending;
	
	public static Type createFromString(String asString) {
		
		if("subscribed".equals(asString)) {
			return subscribed;
		} else if ("unconfigured".equals(asString)) {
			return unconfigured;
		} else if ("pending".equals(asString)) {
			return pending;
		}
		return none;
	}
}
