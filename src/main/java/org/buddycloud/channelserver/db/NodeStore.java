package org.buddycloud.channelserver.db;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.NodeThread;
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
	 * Delete a node
	 *
	 * @param nodeId
	 *            the node reference
	 * @throws NodeStoreException
	 */
	void deleteNode(String nodeId) throws NodeStoreException;
	
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
     * Delete the configuration for a node
     * 
     * @param nodeId
     *            the node id.
     * @throws NodeStoreException
     */
	void deleteNodeConfiguration(String nodeId) throws NodeStoreException;
	
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
     * Get given user's affiliations using RSM
     * 
     * @param actorJid
     * @param maxItemsToReturn
     * @param afterItemId
     * @return
     */
	ResultSet<NodeAffiliation> getUserAffiliations(JID actorJid, 
			String afterItemId, int maxItemsToReturn) throws NodeStoreException;
	
	/**
	 * Get affiliation changes for a user
	 * 
	 * @param user
	 * @param startDate
	 * @param endDate
	 * @return
	 */
	ResultSet<NodeAffiliation> getAffiliationChanges(JID user, Date startDate, Date endDate)
	    throws NodeStoreException;
	
    /**
     * Count the number of affiliations for a user
     * 
     * @param actorJid
     * @return
     */
	int countUserAffiliations(JID actorJid) throws NodeStoreException;
	
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
     * Gets affiliations for a node with RSM 
     * @param node
     * @param maxItemsToReturn
     * @param afterItemId
     * @return
     */
	ResultSet<NodeAffiliation> getNodeAffiliations(String node
			, String afterItemId, int maxItemsToReturn) throws NodeStoreException;
	
	/**
	 * Get a list of node owners
	 * 
	 * @param node
	 * @throws NodeStoreException 
	 */
	ArrayList<JID> getNodeOwners(String node) throws NodeStoreException;
	
	/**
	 * Count the number of affiliations for a node
	 * 
	 * @param nodeId
	 * @return
	 */  
	int countNodeAffiliations(String nodeId) throws NodeStoreException;
	
	/**
	 * Gets the set of nodes to which the user is subscribed.
	 * 
	 * @param user
	 *            the user's JID
	 * @param afterNodeId
	 * @param maxItemsToReturn 
	 * @param user 
	 * @return
	 */
	ResultSet<NodeSubscription> getUserSubscriptions(JID user, String afterNodeId, int maxItemsToReturn)
			throws NodeStoreException;

	/**
	 * Gets the set of nodes to which the user is subscribed.
	 * 
	 * @param user
	 *            the user's JID
	 * @param user 
	 * @return
	 */
	ResultSet<NodeSubscription> getUserSubscriptions(JID user)
			throws NodeStoreException;

	/**
	 * Gets subscription changes for a user
	 * 
	 * @param user
	 * @param startDate
	 * @param endDate
	 * @return
	 * @throws NodeStoreException 
	 */
	ResultSet<NodeSubscription> getSubscriptionChanges(JID user,
			Date startDate, Date endDate) throws NodeStoreException;
	
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
	 * Gets the set of subscriptions to the node.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @param afterItemId
	 *            after this JID
	 * @return
	 */
	ResultSet<NodeSubscription> getNodeSubscriptions(String nodeId, JID afterItemId,
			int maxItemsToReturn) throws NodeStoreException;
	
	/**
	 * Gets the set of listeners to the node.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @return
	 */
	ResultSet<NodeSubscription> getNodeSubscriptionListeners(String nodeId)
			throws NodeStoreException;

	/**
	 * Gets the set of listeners to all nodes.
	 * 
	 * @param nodeId
	 *            the node reference.
	 * @return
	 */
	ResultSet<NodeSubscription> getNodeSubscriptionListeners()
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
	 * Retrieves the number of user subscriptions
	 * 
	 * @param user
	 *            the user's JID from which to retrieve the item count.
	 * @return the entries count.
	 * @throws NodeStoreException
	 */
	int countUserSubscriptions(JID nodeId) throws NodeStoreException;
	
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
	 * Get node items between two dates
	 * 
	 * @param nodeId
	 *            the node id from which to retrieve items
	 * @param startDate
	 *            items older than (or same age as) start date
	 * @param endDate
	 *            items younger than (or same age as) end date
	 */
	CloseableIterator<NodeItem> getNewNodeItemsForUser(JID user,
			Date startDate, Date endDate) throws NodeStoreException;
	
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
     * Retrieves node item replies
     * 
     * @param nodeId
     * @param itemId
     * @param afterItemId
     * @param limit
     * @return
     * @throws NodeStoreException
     */
	ClosableIteratorImpl<NodeItem> getNodeItemReplies(String nodeId, String itemId,
			String afterItemId, int limit) throws NodeStoreException;

	/**
	 * Get a count of the number of replies to an item 
	 * 
	 * @param nodeId
	 * @param itemId
	 * @throws NodeStoreException 
	 */
	int getCountNodeItemReplies(String nodeId, String itemId) throws NodeStoreException;
	
    /**
     * Retrieves node item thread
     * 
     * @param nodeId
     * @param itemId
     * @param afterItemId
     * @param limit
     * @return
     * @throws NodeStoreException
     */
	ClosableIteratorImpl<NodeItem> getNodeItemThread(String nodeId, String itemId,
			String afterItemId, int limit) throws NodeStoreException;
	
	/**
	 * Get a count of the number of items in a thread
	 * 
	 * @param nodeId
	 * @param itemId
	 * @throws NodeStoreException 
	 */
	int getCountNodeThread(String nodeId, String itemId)
			throws NodeStoreException;
	
    /**
     * Get recent items for a user
     * 
     * @param user
     * @param since
     * @param maxPerNode
     * @return
     * @throws NodeStoreException
     */
	CloseableIterator<NodeItem> getRecentItems(JID user, Date since,
			int maxPerNode, int limit, GlobalItemID afterItemId, String node) throws NodeStoreException;
	
	/**
	 * Get count of recent items for a user
	 * @throws NodeStoreException 
	 */
	int getCountRecentItems(JID user, Date since, int maxPerNode, String node) throws NodeStoreException;

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
	 * Retrieves the number of subscriptions to a node
	 * 
	 * @param nodeId
	 *            the node id from which to retrieve the item count.
	 * @return the entries count.
	 * @throws NodeStoreException
	 */
	int countNodeSubscriptions(String nodeId) throws NodeStoreException;
	
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
	 * Allows the server to determine if the requested node is cached locally
	 * 
	 * @param nodeId
	 *            the node id.
	 */
	boolean isCachedNode(String nodeId) throws NodeStoreException;

	/**
	 * Allows the server to determine if the requested JID details are cached locally
	 * 
	 * @param jid
	 *            user/node jid.
	 */
	boolean isCachedJID(JID jid) throws NodeStoreException;
	
	
	/**
	 * Allows the server to determine if the requested (subscriptions) node 
	 * is cached locally
	 * 
	 * @param nodeId
	 *            the node id.
	 */
	boolean nodeHasSubscriptions(String nodeId) throws NodeStoreException;
	
    /**
     * Purges all items from a node
     * 
     * @param nodeId
     *            the node id
     * @throws NodeStoreException 
     */
	void purgeNodeItems(String nodeId) throws NodeStoreException;
	
	/**
	 * Retrieves a list of nodes
	 * @throws NodeStoreException 
	 */
	ArrayList<String> getNodeList() throws NodeStoreException;
	
    /**
     * Search subscribed nodes for content
     * 
     * @param searcher  JID of user performing the search
     * @param content   Keywords upon which to search
     * @param author    JID of the content author
     * @param page      Page number of results (>= 1)
     * @param rpp       Results per page (>= 1)
     * @return
     * @throws NodeStoreException 
     */
	CloseableIterator<NodeItem> performSearch(JID searcher, List content,
			JID author, int page, int rpp) throws NodeStoreException;
	
	/**
	 * Retrieves a list of items from public channels "firehose"
     * 
     * @param limit
     *            limit the number of results
     * @param afterItemId
     *            after item ID#
     * @param isAdmin
     *            show items from non-open nodes
     * @return
     * @throws NodeStoreException
     */
	CloseableIterator<NodeItem> getFirehose(int limit, String afterItemId, boolean isAdmin) throws NodeStoreException;
	
	/**
	 * Get count of items from public channels "firehose"
	 * 
	 * @param isAdmin
	 *           counts items from non-open nodes
	 * @throws NodeStoreException 
	 */
	int getFirehoseItemCount(boolean isAdmin) throws NodeStoreException;
	
	/**
	 * Get a list of posts for a user
	 * 
	 * @param userJid
	 * @return
	 * @throws NodeStoreException
	 */
	ResultSet<NodeItem> getUserItems(JID userJid) throws NodeStoreException;
	
	/**
	 * Delete user posts
	 * 
	 * @param userJid
	 * @throws NodeStoreException
	 */
	void deleteUserItems(JID userJid) throws NodeStoreException;
	
	/**
	 * Delete affiliations for a user
	 * 
	 * @param userJid
	 * @throws NodeStoreException
	 */
	void deleteUserAffiliations(JID userJid) throws NodeStoreException;
	
	/**
	 * Delete user subscriptions
	 * 
	 * @param userJid
	 * @throws NodeStoreException
	 */
	void deleteUserSubscriptions(JID userJid) throws NodeStoreException;
	
	/**
	 * Get node threads
	 * 
	 * @param node
	 * @param afterId
	 * @param limit
	 * @return
	 * @throws NodeStoreException
	 */
	ResultSet<NodeThread> getNodeThreads(String node, String afterId, int limit) throws NodeStoreException;
	
	/**
	 * Count node threads
	 * 
	 * @param node
	 * @return
	 * @throws NodeStoreException
	 */
	int countNodeThreads(String node) throws NodeStoreException;
	
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
	}
}