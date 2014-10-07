package org.buddycloud.channelserver.pubsub.model;

import java.util.Date;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.Result;

/**
 * Represents a user's affiliation with a node
 */
public interface NodeAffiliation extends Result {
    /**
     * Gets the user who is affiliated with the node
     * 
     * @return the JID
     */
    JID getUser();

    /**
     * Gets the node to which the user is affiliated
     * 
     * @return
     */
    String getNodeId();

    /**
     * The type of affiliation
     * 
     * @return
     */
    Affiliations getAffiliation();

    /**
     * Gets the date this affiliation was last updated
     * 
     * @return
     */
    Date getLastUpdated();
}
