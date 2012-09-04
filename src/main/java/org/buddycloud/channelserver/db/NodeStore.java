package org.buddycloud.channelserver.db;

import java.util.Collection;
import java.util.Map;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.affiliation.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.Node;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.xmpp.packet.JID;

/**
 * NodeStore is a interface for classes which have the ability to store and retrieve nodes,
 * and user affiliations and subscriptions to those nodes.
 */
public interface NodeStore {
	/**
	 * Create a new node.
	 * @param owner the node owner.
	 * @param nodeId the node reference.
	 * @param nodeConf a configuration map for the node.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	void createNode(JID owner, String nodeId, Map<String,String> nodeConf) throws NodeStoreException;
	
	/**
	 * Sets a single configuration option on the node
	 * @param nodeId the node reference
	 * @param key the configuration option key
	 * @param value the configuration option value
	 */
	void setNodeConf(String nodeId, String key, String value);
	
	/**
	 * Retrieves a node from the data store.
	 * @param nodeId the node reference.
	 * @return the Node, or <code>null</code> if the node does not exist.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	Node fetchNode(String nodeId) throws NodeStoreException;
	
	/**
	 * Determine whether a node exists within the data store.
	 * @param nodeId the node reference.
	 * @return <code>true</code> if the node exists, <code>false</code> otherwise.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	boolean nodeExists(String nodeId) throws NodeStoreException;
	
	/**
	 * Sets a user's affiliation with a node. If the user doesn't already have an affiliation with the node
	 * then one will be added.
	 * @param nodeId the node reference.
	 * @param user the user.
	 * @param affiliation the type of affiliation.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	void setUserAffiliation(String nodeId, JID user, Affiliations affiliation) throws NodeStoreException;
	
	/**
	 * Adds a user's subscription to the node. If the user already has a subscription then the existing one will be updated.
	 * @param nodeId the node reference.
	 * @param user the user's JID.
	 * @param subscriptionId an id for the user's subscription (to allow a user to have multiple subscriptions to a single node).
	 * @param subscription the type of subscription.
	 * @throws NodeStoreException if the operation could not be completed.
	 */
	void addUserSubscription(NodeSubscription subscription) throws NodeStoreException;
	
	/**
	 * Gets a user's affiliation with a node
	 * @param nodeId the node reference.
	 * @param user the user's JID
	 * @return
	 */
	NodeAffiliation getUserAfilliation(String nodeId, JID user) throws NodeStoreException;
	
	/**
	 * Gets the set of nodes to which the user is subscribed
	 * @param nodeId the node reference.
	 * @param user the user's JID
	 * @return
	 */
	Collection<NodeSubscription> getUserSubscriptions(String nodeId, JID user) throws NodeStoreException;

	/**
	 * Begins an atomic transaction. The transaction will include any operations carried out on this object
	 * until either {@link #commitTransaction()} or {@link #rollbackTransaction()} is called.
	 * 
	 * @throws IllegalStateException if a transaction is already in progress.
	 */
	void beginTransaction();
	
	/**
	 * Commits and closes the current transaction.
	 * 
	 * @throws IllegalStateException if there is no current transaction.
	 */
	void commitTransaction();
	
	/**
	 * Closes and rolls back the current transaction.
	 * <p>Silently fails if there is no current transaction so that it can safely be used in a finally block. e.g:
	 * <p><blockquote><pre>
	 * try {
	 *   nodeStore.beginTransaction();
	 * 
	 *   ... Do some stuff ...
	 *   
	 *   nodeStore.commitTransaction();
	 * } finally {
	 *   nodeStore.closeTransaction();
	 * }
	 * </pre></blockquote>
	 */
	void closeTransaction();
}
