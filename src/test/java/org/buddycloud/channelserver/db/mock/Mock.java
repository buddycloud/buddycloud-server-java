package org.buddycloud.channelserver.db.mock;

import java.util.Collection;
import java.util.Map;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.xmpp.packet.JID;

public class Mock implements ChannelManager {

	// Store last provided configuration
	private Map<String, String> conf;
	// Store last provided node subscription
	private NodeSubscription nodeSubscription;
    // Store last provided node affiliation
	private Affiliations nodeAffiliation;	
	
	/**
	 * Helper methods
	 */
	public NodeSubscription getLastUserNodeSubscription() {
		return nodeSubscription;
	}
	
	public Affiliations getLastUserNodeAffiliation() {
		return nodeAffiliation;
	}
	
	/**
	 * Standard methods
	 */
	
	@Override
	public void createNode(JID owner, String nodeId,
			Map<String, String> nodeConf) throws NodeStoreException {
		// TODO Auto-generated method stub
		conf = nodeConf;
	}

	@Override
	public void setNodeConfValue(String nodeId, String key, String value)
			throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setNodeConf(String nodeId, Map<String, String> conf)
			throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public String getNodeConfValue(String nodeId, String key)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, String> getNodeConf(String nodeId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return conf;
	}

	@Override
	public boolean nodeExists(String nodeId) throws NodeStoreException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setUserAffiliation(String nodeId, JID user,
			Affiliations affiliation) throws NodeStoreException {
		// TODO Auto-generated method stub
		nodeAffiliation = affiliation;
	}

	@Override
	public void addUserSubscription(NodeSubscription subscription)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		nodeSubscription = subscription;
	}

	@Override
	public NodeAffiliation getUserAffiliation(String nodeId, JID user)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<NodeAffiliation> getUserAffiliations(JID user)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<NodeAffiliation> getNodeAffiliations(String nodeId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<NodeSubscription> getUserSubscriptions(JID user)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<NodeSubscription> getNodeSubscriptions(String nodeId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSubscription getUserSubscription(String nodeId, JID user)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CloseableIterator<NodeItem> getNodeItems(String nodeId,
			String afterItemId, int count) throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CloseableIterator<NodeItem> getNodeItems(String nodeId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int countNodeItems(String nodeId) throws NodeStoreException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public NodeItem getNodeItem(String nodeId, String nodeItemId)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void addNodeItem(NodeItem item) throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void updateNodeItem(NodeItem item) throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void deleteNodeItemById(String nodeId, String nodeItemId)
			throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void close() throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public Transaction beginTransaction() throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void createPersonalChannel(JID ownerJID) throws NodeStoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean isLocalNode(String nodeId) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isLocalJID(JID jid) {
		// TODO Auto-generated method stub
		return false;
	}

}
