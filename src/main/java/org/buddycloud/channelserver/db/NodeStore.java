package org.buddycloud.channelserver.db;

import java.util.Map;
import java.util.Set;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.affiliation.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.Node;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

/**
 * NodeStore is a interface for classes which have the ability to store and retrieve nodes
 * and user affiliations and subscriptions to those nodes.
 */
public interface NodeStore {
	/**
	 * Create a new node.
	 * @param owner the node owner.
	 * @param nodeID the node id.
	 * @param nodeConf a configuration map for the node.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	void createNode(JID owner, String nodeID, Map<String,String> nodeConf) throws NodeStoreException;
	
	/**
	 * Retrieves a node from the data store.
	 * @param nodeID the node id
	 * @return the Node, or <code>null</code> if the node does not exist.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	Node fetchNode(String nodeID) throws NodeStoreException;
	
	/**
	 * Determine whether a node exists within the data store.
	 * @param nodeID the node id.
	 * @return <code>true</code> if the node exists, <code>false</code> otherwise.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	boolean nodeExists(String nodeID) throws NodeStoreException;
	
	/**
	 * Sets a user's affiliation with a node. If the user doesn't already have an affiliation with the node
	 * then one will be added.
	 * @param nodeID the node id.
	 * @param user the user.
	 * @param affiliation the type of affiliation.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	void setUserAffiliation(String nodeID, JID user, Affiliations affiliation) throws NodeStoreException;
	
	/**
	 * Sets a user's subscription to a node. If the user doesn't already have a subscription then one will be added.
	 * @param nodeID the node id.
	 * @param user the user's JID.
	 * @param subscriptionId an id for the user's subscription (to allow a user to have multiple subscriptions to a single node).
	 * @param subscription the type of subscription.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	void setUserSubscription(String nodeID, JID user, String subscriptionId, Subscriptions subscription) throws NodeStoreException;
	
	NodeAffiliation getUserAfilliation(String nodeID, JID user);
	
	Set<NodeSubscription> getUserSubscriptions(String nodeID, JID user);
}
