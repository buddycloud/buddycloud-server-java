package org.buddycloud.channelserver.channel;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.NodeThread;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.ResultSet;

/**
 * A basic implementation of a channel manager which delegates pretty much
 * everything to a {@link NodeStore}.
 */
public class ChannelManagerImpl implements ChannelManager {

	private final NodeStore nodeStore;

	private static final Logger logger = Logger
			.getLogger(ChannelManagerImpl.class);

	private static final String REMOTE_NODE = "Illegal remote node";

	/**
	 * Create an instance backed by a {@link NodeStore}.
	 * 
	 * @param nodeStore
	 *            the backing {@link NodeStore}.
	 */
	public ChannelManagerImpl(final NodeStore nodeStore) {
		this.nodeStore = nodeStore;
	}

	@Override
	public void createNode(JID owner, String nodeId,
			Map<String, String> nodeConf) throws NodeStoreException {
		nodeStore.createNode(owner, nodeId, nodeConf);
	}

	@Override
	public void setNodeConfValue(String nodeId, String key, String value)
			throws NodeStoreException {
		nodeStore.setNodeConfValue(nodeId, key, value);
	}

	@Override
	public void setNodeConf(String nodeId, Map<String, String> conf)
			throws NodeStoreException {
		nodeStore.setNodeConf(nodeId, conf);
	}

	@Override
	public String getNodeConfValue(String nodeId, String key)
			throws NodeStoreException {
		return nodeStore.getNodeConfValue(nodeId, key);
	}

	@Override
	public void deleteNodeConfiguration(String nodeId)
			throws NodeStoreException {
		nodeStore.deleteNodeConfiguration(nodeId);
	}

	@Override
	public Map<String, String> getNodeConf(String nodeId)
			throws NodeStoreException {
		return nodeStore.getNodeConf(nodeId);
	}

	@Override
	public boolean isCachedNodeConfig(String nodeId) throws NodeStoreException {
		return nodeStore.isCachedNodeConfig(nodeId);
	}

	@Override
	public boolean nodeExists(String nodeId) throws NodeStoreException {
		return nodeStore.nodeExists(nodeId);
	}

	@Override
	public void setUserAffiliation(String nodeId, JID user,
			Affiliations affiliation) throws NodeStoreException {
		nodeStore.setUserAffiliation(nodeId, user, affiliation);
	}

	@Override
	public void addUserSubscription(NodeSubscription subscription)
			throws NodeStoreException {
		nodeStore.addUserSubscription(subscription);
	}

	@Override
	public ResultSet<NodeAffiliation> getAffiliationChanges(JID user,
			Date startDate, Date endDate) throws NodeStoreException {
		return nodeStore.getAffiliationChanges(user, startDate, endDate);
	}

	@Override
	public ArrayList<JID> getNodeOwners(String node) throws NodeStoreException {
		return nodeStore.getNodeOwners(node);
	}

	@Override
	public ResultSet<NodeSubscription> getSubscriptionChanges(JID user,
			Date startDate, Date endDate) throws NodeStoreException {
		return nodeStore.getSubscriptionChanges(user, startDate, endDate);
	}

	@Override
	public CloseableIterator<NodeItem> getNodeItems(String nodeId,
			String afterItemId, int count) throws NodeStoreException {
		return nodeStore.getNodeItems(nodeId, afterItemId, count);
	}

	@Override
	public CloseableIterator<NodeItem> getNodeItems(String nodeId)
			throws NodeStoreException {
		return nodeStore.getNodeItems(nodeId);
	}

	@Override
	public ClosableIteratorImpl<NodeItem> getNodeItemReplies(String nodeId,
			String itemId, String afterItemId, int limit)
			throws NodeStoreException {
		return nodeStore.getNodeItemReplies(nodeId, itemId, afterItemId, limit);
	}

	@Override
	public int getCountNodeItemReplies(String nodeId, String itemId)
			throws NodeStoreException {
		return nodeStore.getCountNodeItemReplies(nodeId, itemId);
	}

	@Override
	public ClosableIteratorImpl<NodeItem> getNodeItemThread(String nodeId,
			String itemId, String afterItemId, int limit)
			throws NodeStoreException {
		return nodeStore.getNodeItemThread(nodeId, itemId, afterItemId, limit);
	}

	@Override
	public int getCountNodeThread(String nodeId, String itemId)
			throws NodeStoreException {
		return nodeStore.getCountNodeThread(nodeId, itemId);
	}

	@Override
	public CloseableIterator<NodeItem> getNewNodeItemsForUser(JID user,
			Date startDate, Date endDate) throws NodeStoreException {
		return nodeStore.getNewNodeItemsForUser(user, startDate, endDate);
	}

	@Override
	public int countNodeItems(String nodeId) throws NodeStoreException {
		return nodeStore.countNodeItems(nodeId);
	}

	public boolean isCachedNode(String nodeId) throws NodeStoreException {
		return nodeStore.isCachedNode(nodeId);
	}

	@Override
	public boolean nodeHasSubscriptions(String nodeId)
			throws NodeStoreException {
		return (nodeStore.getNodeMemberships(nodeId).size() > 0);
	}

	public boolean isCachedJID(JID jid) throws NodeStoreException {
		return nodeStore.isCachedJID(jid);
	}

	@Override
	public NodeItem getNodeItem(String nodeId, String nodeItemId)
			throws NodeStoreException {
		return nodeStore.getNodeItem(nodeId, nodeItemId);
	}
	
	@Override
	public NodeItem getNodeItem(GlobalItemID item) throws NodeStoreException {
		return nodeStore.getNodeItem(item);
	}

	@Override
	public void addNodeItem(NodeItem item) throws NodeStoreException {
		nodeStore.addNodeItem(item);
	}

	@Override
	public void updateNodeItem(NodeItem item) throws NodeStoreException {
		nodeStore.updateNodeItem(item);
	}

	@Override
	public void deleteNodeItemById(String nodeId, String nodeItemId)
			throws NodeStoreException {
		nodeStore.deleteNodeItemById(nodeId, nodeItemId);
	}

	@Override
	public Transaction beginTransaction() throws NodeStoreException {
		return nodeStore.beginTransaction();
	}

	@Override
	public void createPersonalChannel(JID owner) throws NodeStoreException {
		if (false == Configuration.getInstance().isLocalJID(owner)) {
			throw new IllegalArgumentException(REMOTE_NODE);
		}
		if (!nodeExists(Conf.getPostChannelNodename(owner))) {
			this.createNode(owner, Conf.getPostChannelNodename(owner),
					Conf.getDefaultPostChannelConf(owner));
		}
		if (!nodeExists(Conf.getStatusChannelNodename(owner))) {
			this.createNode(owner, Conf.getStatusChannelNodename(owner),
					Conf.getDefaultStatusChannelConf(owner));
		}
		if (!nodeExists(Conf.getSubscriptionsChannelNodename(owner))) {
			this.createNode(owner, Conf.getSubscriptionsChannelNodename(owner),
					Conf.getDefaultSubscriptionsChannelConf(owner));
		}
		if (!nodeExists(Conf.getGeoPreviousChannelNodename(owner))) {
			this.createNode(owner, Conf.getGeoPreviousChannelNodename(owner),
					Conf.getDefaultGeoPreviousChannelConf(owner));
		}
		if (!nodeExists(Conf.getGeoCurrentChannelNodename(owner))) {
			this.createNode(owner, Conf.getGeoCurrentChannelNodename(owner),
					Conf.getDefaultGeoCurrentChannelConf(owner));
		}
		if (!nodeExists(Conf.getGeoNextChannelNodename(owner))) {
			this.createNode(owner, Conf.getGeoNextChannelNodename(owner),
					Conf.getDefaultGeoNextChannelConf(owner));
		}
	}

	@Override
	public void close() throws NodeStoreException {
		nodeStore.close();
	}

	@Override
	public void addRemoteNode(String node) throws NodeStoreException {
		nodeStore.addRemoteNode(node);
	}

	@Override
	public ResultSet<NodeSubscription> getNodeSubscriptionListeners(
			String nodeId) throws NodeStoreException {
		return nodeStore.getNodeSubscriptionListeners(nodeId);
	}

	@Override
	public ResultSet<NodeSubscription> getNodeSubscriptionListeners()
			throws NodeStoreException {
		return nodeStore.getNodeSubscriptionListeners();
	}

	@Override
	public void deleteNode(String nodeId) throws NodeStoreException {
		nodeStore.deleteNode(nodeId);
	}

	@Override
	public void deleteRemoteData() throws NodeStoreException {
		List<String> nodes = this.getRemoteNodesList();
		for (String node : nodes) {
			try {
				if (true == node.equals(("/firehose"))) {
					continue;
				}
				nodeStore.purgeNodeItems(node);
			} catch (IllegalArgumentException e) {
				logger.error("Invalid remote node in datastore " + node, e);
			}
		}
	}

	@Override
	public void purgeNodeItems(String nodeId) throws NodeStoreException {
		nodeStore.purgeNodeItems(nodeId);
	}

	@Override
	public ArrayList<String> getNodeList() throws NodeStoreException {
		return nodeStore.getNodeList();
	}

	@Override
	public CloseableIterator<NodeItem> getRecentItems(JID user, Date since,
			int maxPerNode, int limit, GlobalItemID afterItemId, String node,
			boolean parentOnly) throws NodeStoreException {
		return nodeStore.getRecentItems(user, since, maxPerNode, limit,
				afterItemId, node, parentOnly);
	}

	@Override
	public int getCountRecentItems(JID user, Date since, int maxPerNode,
			String node, boolean parentOnly) throws NodeStoreException {
		return nodeStore.getCountRecentItems(user, since, maxPerNode, node,
				parentOnly);
	}

	@Override
	public CloseableIterator<NodeItem> getFirehose(int limit,
			String afterItemId, boolean isAdmin, String actorDomain) throws NodeStoreException {
		return nodeStore.getFirehose(limit, afterItemId, isAdmin, actorDomain);
	}

	@Override
	public int getFirehoseItemCount(boolean isAdmin, String actorDomain) throws NodeStoreException {
		return nodeStore.getFirehoseItemCount(isAdmin, actorDomain);
	}

	@Override
	public Affiliations getDefaultNodeAffiliation(String nodeId)
			throws NodeStoreException {
		String affiliationString = getNodeConfValue(nodeId,
				Conf.DEFAULT_AFFILIATION);

		if (affiliationString != null) {
			try {
				return Affiliations.valueOf(affiliationString);
			} catch (IllegalArgumentException e) {
				logger.error("Invalid default affiliation stored for node "
						+ nodeId + ": " + affiliationString, e);
			}
		}

		return Affiliations.member;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public CloseableIterator<NodeItem> performSearch(JID searcher,
			List content, JID author, int page, int rpp)
			throws NodeStoreException {
		return nodeStore.performSearch(searcher, content, author, page, rpp);
	}

	@Override
	public ResultSet<NodeItem> getUserPublishedItems(JID userJid)
			throws NodeStoreException {
		return nodeStore.getUserPublishedItems(userJid);
	}

	@Override
	public void deleteUserItems(JID userJid) throws NodeStoreException {
		nodeStore.deleteUserItems(userJid);
	}

	@Override
	public void deleteUserAffiliations(JID userJid) throws NodeStoreException {
		nodeStore.deleteUserAffiliations(userJid);
	}

	@Override
	public void deleteUserSubscriptions(JID userJid) throws NodeStoreException {
		nodeStore.deleteUserSubscriptions(userJid);
	}

	@Override
	public ResultSet<NodeThread> getNodeThreads(String node, String afterId,
			int limit) throws NodeStoreException {
		return nodeStore.getNodeThreads(node, afterId, limit);
	}

	@Override
	public int countNodeThreads(String node) throws NodeStoreException {
		return nodeStore.countNodeThreads(node);
	}

	@Override
	public boolean userHasRatedPost(String node, JID user, GlobalItemID id)
			throws NodeStoreException {
		return nodeStore.userHasRatedPost(node, user, id);
	}

	@Override
	public void updateThreadParent(String node, String itemId)
			throws NodeStoreException {
		nodeStore.updateThreadParent(node, itemId);
		
	}

	@Override
	public NodeMembership getNodeMembership(String nodeId, JID user)
			throws NodeStoreException {
		return nodeStore.getNodeMembership(nodeId, user);
	}
	
	@Override
	public ResultSet<NodeMembership> getUserMemberships(JID jid)
			throws NodeStoreException {
		return nodeStore.getUserMemberships(jid);
	}
	
	@Override
	public ResultSet<NodeMembership> getNodeMemberships(String nodeId)
	    throws NodeStoreException {
		return nodeStore.getNodeMemberships(nodeId);
	}
	
	@Override
	public CloseableIterator<NodeItem> getUserFeedItems(JID user, Date since,
		 int limit, GlobalItemID afterItemId, boolean parentOnly) throws NodeStoreException {
		return nodeStore.getUserFeedItems(user, since, limit, afterItemId, parentOnly);
	}
	
	@Override
	public int getCountUserFeedItems(JID user, Date since,
			boolean parentOnly) throws NodeStoreException {
		return nodeStore.getCountUserFeedItems(user, since, parentOnly);
	}

	@Override
	public void jidOnline(JID jid) throws NodeStoreException {
		nodeStore.jidOnline(jid);
	}

	@Override
	public void jidOffline(JID jid) throws NodeStoreException {
		nodeStore.jidOffline(jid);
	}

	@Override
	public ArrayList<JID> onlineJids(JID jid) throws NodeStoreException {
		return nodeStore.onlineJids(jid);
	}

	@Override
	public List<String> getLocalNodesList() throws NodeStoreException {
		return nodeStore.getLocalNodesList();
	}

	@Override
	public List<String> getRemoteNodesList() throws NodeStoreException {
		return nodeStore.getRemoteNodesList();
	}

}