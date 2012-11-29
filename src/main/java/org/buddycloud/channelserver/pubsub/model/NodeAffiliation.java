package org.buddycloud.channelserver.pubsub.model;

import org.buddycloud.channelserver.node.NodeRef;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.Result;

/**
 * Represents a user's affiliation with a node
 */
public abstract class NodeAffiliation implements Result {
	/**
	 * Gets the user who is affiliated with the node
	 * @return the JID
	 */
	public abstract JID getUser();
	
	/**
	 * Gets the node to which the user is affiliated
	 * @return
	 */
	public abstract String getNodeId();
	
	/**
	 * The type of affiliation
	 * @return
	 */
	public abstract Affiliations getAffiliation();
}