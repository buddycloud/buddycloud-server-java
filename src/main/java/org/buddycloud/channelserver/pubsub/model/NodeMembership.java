package org.buddycloud.channelserver.pubsub.model;

import java.util.Date;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.Result;

/**
 * Represents a user's subscription to a node
 */
public interface NodeMembership extends Result {

	/**
	 * Gets the subscription type
	 * @return
	 */
    Subscriptions getSubscription();
    
    /**
     * Gets the affiliation type
     * @return
     */
    Affiliations getAffiliation();
    
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
    
    /**
     * Get last updated
     * @return
     */
    Date getLastUpdated();

    /**
     * Get the inviter
     * 
     * @return
     */
	JID getInviter();
}
