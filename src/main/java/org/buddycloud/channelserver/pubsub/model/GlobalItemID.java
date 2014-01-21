package org.buddycloud.channelserver.pubsub.model;

import org.xmpp.packet.JID;

/**
 * Pointer to a globally identifiable node item, using a service JID, node ID
 * and itemID. The Item is unique within the Node The Node is unique within the
 * Service The Service is unique within the network.
 */
public interface GlobalItemID {
	/**
	 * Gets the JID of service which hosts the node (and item)
	 * 
	 * @return the JID of the service.
	 */
	public JID getService();

	/**
	 * Gets the NodeID of the node containing the item
	 * 
	 * @return the NodeID of the node.
	 */
	public String getNodeID();

	/**
	 * Gets the ItemID of the item within the Node
	 * 
	 * @return the ItemID if the item.
	 */
	public String getItemID();
}
