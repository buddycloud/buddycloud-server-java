package org.buddycloud.channelserver.db;

import java.util.Iterator;
import java.util.Map;

import org.buddycloud.channelserver.db.exception.NodeNotFoundException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.ResultSet;

/**
 * NodeStore is a interface for classes which have the ability to store and
 * retrieve nodes, and user affiliations and subscriptions to those nodes.
 */
public interface NodeStore {
	/**
	 * Create a new node.
	 * 
	 * @param owner
	 *            the node owner.
	 * @param nodeId
	 *            the node reference.
	 * @param nodeConf
	 *            a configuration map for the node.
	 * @throws NodeStoreException
	 *             if the operation could not be completed.
	 */
	void createNode(JID owner, String nodeId, Map<String, String> nodeConf)
			throws NodeStoreException;

	/**
	 * Add a remote node into the database
	 * 
	 * @param nodeId
	 *          the node reference
	 * @throws NodeStoreException
	 *             if the operation could not be completed.
	 */
	void addRemoteNode(String node) throws NodeStoreException;
	
	/**
	 * Sets a single configuration option on the node
	 * 
	 * @param nodeId
	 *            the node reference
	 * @param key
	 *            the configuration option key
	 * @param value
	 *            the configuration option value
	 * @throws NodeStoreException
	 */
	void setNodeConfValue(String nodeId, String key, String value)
			throws NodeStoreException;

	/**
	 * Replaces the existing node configuration.
	 * 
	 * @param nodeId
	 *            the node id
	 * @param conf
	 *            the configuration to replace the current configuration with.
	 * @throws NodeStoreException
	 */
	void setNodeConf(String nodeId, Map<String, String> conf)
			throws NodeStoreException;

	/**
	 * Retrieves a single node configuration parameter.
	 * 
	 * @param nodeId
	 *            the node id.
	 * @param key
	 *            the configuration key.
	 * @return the configuration value, or null if not found.
	 * @throws NodeStoreException
	 */
	String getNodeConfValue(String nodeId, String key)
			throws NodeStoreException;

	/**
	 * Retrieves a map of all all the configuration properties for the node.
	 * 
	 * @param nodeId
	 *            the node id.
	 * @return the node configuration.
	 * @throws NodeStoreException
	 */
	Map<String, String> getNodeConf(String nodeId) throws NodeStoreException;

	/**
	 * Determine whether a node exists within the data store.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @return <code>true</code> if the node exists, <code>false</code>
	 *         otherwise.
	 * @throws NodeStoreException
	 *             if the operation could not be completed.
	 */
	boolean nodeExists(String nodeId) throws NodeStoreException;

	/**
	 * Sets a user's affiliation with a node. If the user doesn't already have
	 * an affiliation with the node then one will be added.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @param user
	 *            the user.
	 * @param affiliation
	 *            the type of affiliation.
	 * @throws NodeStoreException
	 *             if the operation could not be completed.
	 */
	void setUserAffiliation(String nodeId, JID user, Affiliations affiliation)
			throws NodeStoreException;

	/**
	 * Adds a user's subscription to the node. If the user already has a
	 * subscription then the existing one will be updated.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @param user
	 *            the user's JID.
	 * @param subscriptionId
	 *            an id for the user's subscription (to allow a user to have
	 *            multiple subscriptions to a single node).
	 * @param subscription
	 *            the type of subscription.
	 * @throws NodeStoreException
	 *             if the user already has a subscription with the node.
	 */
	void addUserSubscription(NodeSubscription subscription)
			throws NodeStoreException;

	/**
	 * Gets a user's affiliation with a node.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @param user
	 *            the user's JID
	 * @return
	 */
	NodeAffiliation getUserAffiliation(String nodeId, JID user)
			throws NodeStoreException;

	/**
	 * Gets all the given user's affiliations.
	 * 
	 * @param user
	 *            the user's JID
	 * @return
	 */
	ResultSet<NodeAffiliation> getUserAffiliations(JID user)
			throws NodeStoreException;

	/**
	 * Gets all the affiliations with the node.
	 * 
	 * @param nodeId
	 *            the node id
	 * @return
	 */
	ResultSet<NodeAffiliation> getNodeAffiliations(String nodeId)
			throws NodeStoreException;

	/**
	 * Gets the set of nodes to which the user is subscribed.
	 * 
	 * @param user
	 *            the user's JID
	 * @return
	 */
	ResultSet<NodeSubscription> getUserSubscriptions(JID user)
			throws NodeStoreException;

	/**
	 * Gets the set of subscriptions to the node.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @return
	 */
	ResultSet<NodeSubscription> getNodeSubscriptions(String nodeId)
			throws NodeStoreException;

	/**
	 * Gets the user's subscription to a certain node.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @param user
	 *            the user's JID
	 * @return
	 */
	NodeSubscription getUserSubscription(String nodeId, JID user)
			throws NodeStoreException;

	/**
	 * Retrieves an iterator of items within a node.
	 * 
	 * @param nodeId
	 *            the node id from which to retrieve the items.
	 * @param afterNodeId
	 *            the itemId after which to retrieve items (exclusive, based on
	 *            modified date).
	 * @param count
	 *            the maximum number of records to return.
	 * @return an {@link Iterator} of the node entries.
	 * @throws NodeStoreException
	 */

	CloseableIterator<NodeItem> getNodeItems(String nodeId, String afterItemId,
			int count) throws NodeStoreException;

	/**
	 * Retrieves an iterator of all items within a node.
	 * 
	 * @param nodeId
	 *            the node id from which to retrieve the items.
	 * @return an {@link Iterator} of the node entries.
	 * @throws NodeStoreException
	 */
	CloseableIterator<NodeItem> getNodeItems(String nodeId)
			throws NodeStoreException;

	/**
	 * Retrieves the number of items within a node.
	 * 
	 * @param nodeId
	 *            the node id from which to retrieve the item count.
	 * @return the entries count.
	 * @throws NodeStoreException
	 */
	int countNodeItems(String nodeId) throws NodeStoreException;

	/**
	 * Retrieves a single node item by the node item id.
	 * 
	 * @param nodeId
	 *            the node id.
	 * @param nodeItemId
	 *            the node item id.
	 * @return the node item, or <code>null</code> if not found.
	 * @throws NodeStoreException
	 */
	NodeItem getNodeItem(String nodeId, String nodeItemId)
			throws NodeStoreException;

	/**
	 * Stores a new item against the node.
	 * 
	 * @param item
	 *            the node item.
	 * @throws NodeStoreException
	 *             if an item with the same id already exists against the node.
	 */
	void addNodeItem(NodeItem item) throws NodeStoreException;

	/**
	 * Updates an existing item against the node.
	 * 
	 * @param item
	 *            the node item.
	 * @throws NodeStoreException
	 *             if an item with the same id did not already exist.
	 */
	void updateNodeItem(NodeItem item) throws NodeStoreException;

	/**
	 * Deletes the specified node item.
	 * 
	 * @param nodeId
	 *            the node id.
	 * @param nodeItemId
	 *            the id of the item to delete.
	 * @throws NodeStoreException
	 *             if the item was not found
	 */
	void deleteNodeItemById(String nodeId, String nodeItemId)
			throws NodeStoreException;

	/**
	 * Closes this node store instance and releases any resources.
	 */
	void close() throws NodeStoreException;
	
	/**
	 * Begins an atomic transaction. The transaction will include any operations
	 * carried out on this object until either {@link #commitTransaction()} or
	 * {@link #rollbackTransaction()} is called. Can be called multiple times to
	 * invoke a sort of stack of transactions. The transaction will then only be
	 * committed if {@link #commitTransaction()} has been called the same number
	 * of times that {@link #beginTransaction()} was called. If
	 * {@link #closeTransaction()}
	 * 
	 * @return the transaction object which can be used to commit or rollback
	 *         the transaction.
	 * @throws NodeStoreException
	 * @throws IllegalStateException
	 *             if a failed (i.e. rolled back) transaction is in progress
	 */
	Transaction beginTransaction() throws NodeStoreException;

	/**
	 * A {@link NodeStore} transaction.
	 */
	public interface Transaction {

		/**
		 * Commits and closes the transaction.
		 * 
		 * @throws NodeStoreException
		 * 
		 * @throws IllegalStateException
		 *             if the transaction has already been closed.
		 */
		void commit() throws NodeStoreException;

		/**
		 * Closes and rolls back the transaction.
		 * <p>
		 * Silently fails if the transaction has already been committed so that
		 * it can safely be used in a finally block. e.g:
		 * <p>
		 * <blockquote>
		 * 
		 * <pre>
		 * Transaction transaction = null;
		 * 
		 * try {
		 *   transaction = nodeStore.beginTransaction();
		 * 
		 *   ... Do some stuff ...
		 *   
		 *   transaction.commit();
		 * } finally {
		 *   if(transaction != null) transaction.close();
		 * }
		 * </pre>
		 * 
		 * </blockquote>
		 */
		void close() throws NodeStoreException;
	};
}
