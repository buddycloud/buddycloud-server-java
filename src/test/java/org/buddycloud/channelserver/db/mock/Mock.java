package org.buddycloud.channelserver.db.mock;

import java.util.Iterator;
import java.util.Map;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.entry.NodeEntry;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;


public class Mock implements DataStore
{
	/**
	 * Used for testing only 
	 */
	private Map<String, String> configuration = null;
	
	public Map<String, String> getConfiguration()
	{
		return configuration;
	}
	
	/**
	 * Implemented methods
	 */
	public boolean isLocalNode(String nodename)
	{
		return false;
	}

	public Long addLocalUser(String bareJID)
	{
		return null;
	}

	public boolean isLocalUser(String bareJID)
	{
		return false;
	}

	public String addNodeConf(String nodename, Map<String, String> conf)
	{
		return null;
	}

	public String createUserNodes(String owner)
	{
		return null;
	}

	public void createNode(String owner, String nodename,
		 Map<String, String> conf) throws DataStoreException
	{
		configuration = conf;
	}

	public boolean subscribeUserToNode(String bareJID, String nodename,
			String aff, String subs, String foreignChannelServer)
	{
		return false;
	}

	public boolean unsubscribeUserFromNode(String bareJID, String node)
	{
		return false;
	}

	public Iterator<? extends NodeSubscription> getUserSubscriptionsOfNodes(
			String bareJID)
	{
		return null;
	}

	public NodeSubscriptionImpl getUserSubscriptionOfNode(String bareJID,
			String node)
	{
		return null;
	}

	public Iterator<? extends NodeSubscription> getNodeSubscribers(String node)
	{
		return null;
	}

	public Map<String, String> getNodeConf(String nodename)
	{
		return configuration;
	}

	public Iterator<? extends NodeEntry> getNodeEntries(String node, int limit,
			String afterItemId)
    {
		return null;
	}

	public int getNodeEntriesCount(String node)
	{
		return 0;
	}

	public boolean storeEntry(String nodename, String id, String entry)
	{
		return false;
	}

	public String storeState(String oldID, String newID,
			Map<String, String> state)
    {
		return null;
	}

	public Map<String, String> getState(String id)
	{
		return null;
	}

	public boolean nodeExists(String createNodeId)
	{
		return false;
	}
}