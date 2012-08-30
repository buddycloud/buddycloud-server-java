package org.buddycloud.channelserver.pubsub.affiliation;

import org.buddycloud.channelserver.node.NodeRef;
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
	NodeRef getNodeRef();
	
	/**
	 * The type of affiliation
	 * @return
	 */
	Affiliations getAffiliation();
}
