package org.buddycloud.channelserver.pubsub.model;

import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.Result;

/**
 * Represents a user's subscription to a node
 */
public abstract class NodeSubscription implements Result {

	/**
	 * Gets the subscription type
	 * @return
	 */
	public abstract Subscriptions getSubscription();
    
    /**
     * Gets the user
     * @return
     */
	public abstract JID getUser();
    
    /**
     * Gets the listener for notifications (for the inbox protocol)
     * @return the listener's JID, or <code>null</code> if not a remote subscription.
     */
	public abstract JID getListener();
    
    /**
     * Gets the node id
     * @return
     */
	public abstract String getNodeId();
}