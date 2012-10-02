package org.buddycloud.channelserver.pubsub.model;

import org.buddycloud.channelserver.node.NodeRef;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.xmpp.packet.JID;

/**
 * Represents a user's affiliation with a node
 */
public interface NodeAffiliation {
	/**
	 * Gets the user who is affiliated with the node
	 * @return the JID
	 */
	JID getUser();
	
	/**
	 * Gets the node to which the user is affiliated
	 * @return
	 */
	String getNodeId();
	
	/**
	 * The type of affiliation
	 * @return
	 */
	Affiliations getAffiliation();
}
