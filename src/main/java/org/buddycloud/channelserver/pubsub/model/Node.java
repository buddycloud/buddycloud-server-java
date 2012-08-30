package org.buddycloud.channelserver.pubsub.model;

import java.util.Map;

import org.buddycloud.channelserver.node.NodeRef;
import org.xmpp.packet.JID;

/**
 * Represents a pubsub node within the system 
 */
public interface Node {
	
	/**
	 * Returns the node reference.
	 */
	NodeRef getNodeRef();
	
	/**
	 * Return the JID of the owner of the node.
	 */
	JID getOwner();
	
	/**
	 * Returns the map of the node configuration.
	 */
	Map<String,String> getConfiguration();
}
