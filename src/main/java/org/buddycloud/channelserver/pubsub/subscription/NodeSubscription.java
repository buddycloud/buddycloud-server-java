package org.buddycloud.channelserver.pubsub.subscription;

import org.buddycloud.channelserver.node.NodeRef;
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
     * Gets the node reference
     * @return
     */
    NodeRef getNodeRef();
}
