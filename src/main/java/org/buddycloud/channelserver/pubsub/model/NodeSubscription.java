package org.buddycloud.channelserver.pubsub.model;

import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

/**
 * Represents a user's subscription to a node
 */
public interface NodeSubscription {

	/**
	 * Gets the subscription type
	 * @return
	 */
    Subscriptions getSubscription();
    
    /**
     * Gets the user
     * @return
     */
    JID getUser();
    
    /**
     * Gets the listener for notifications (for the inbox protocol)
     * @return the listener's JID, or <code>null</code> if not a remote subscription.
     */
    JID getListener();
    
    /**
     * Gets the node id
     * @return
     */
    String getNodeId();
}
