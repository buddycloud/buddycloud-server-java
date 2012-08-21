package org.buddycloud.channelserver.db.mock;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.entry.NodeEntry;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscriptionMock;


public class Mock implements DataStore
{
	/**
	 * Used for testing only 
	 */
	private HashMap<String, String> configuration = null;
	
	public HashMap<String, String> getConfiguration() throws DataStoreException
	{
		return configuration;
	}
	
	/**
	 * Implemented methods
	 */
	public boolean isLocalNode(String nodename) throws DataStoreException
	{
		return false;
	}

	public Long addLocalUser(String bareJID) throws DataStoreException
	{
		return null;
	}

	public boolean isLocalUser(String bareJID) throws DataStoreException
	{
		return false;
	}

	public String addNodeConf(String nodename, Map<String, String> conf) 
		throws DataStoreException
	{
		return null;
	}

	public String createUserNodes(String owner) throws DataStoreException
	{
		return null;
	}

	public void createNode(String owner, String nodename,
		 Map<String, String> conf) throws DataStoreException
	{
		configuration = (HashMap<String, String>) conf;
	}

	public boolean subscribeUserToNode(String bareJID, String nodename,
			String aff, String subs, String foreignChannelServer) 
	    throws DataStoreException
	{
		return false;
	}

	public boolean unsubscribeUserFromNode(String bareJID, String node) 
		throws DataStoreException
	{
		return false;
	}

	public Iterator<? extends NodeSubscription> getUserSubscriptionsOfNodes(
			String bareJID) throws DataStoreException
	{
		return null;
	}

	public NodeSubscriptionImpl getUserSubscriptionOfNode(String bareJID,
			String node) throws DataStoreException
	{
		return null;
	}

	public Iterator<? extends NodeSubscription> getNodeSubscribers(String node) 
		throws DataStoreException
	{
		ArrayList<NodeSubscriptionMock> subscribers = new ArrayList<NodeSubscriptionMock>();
		subscribers.add(new NodeSubscriptionMock());
		System.out.println(subscribers.iterator());
		return subscribers.iterator();
	}

	public HashMap<String, String> getNodeConf(String nodename) 
		throws DataStoreException
	{
		return configuration;
	}

	public Iterator<? extends NodeEntry> getNodeEntries(String node, int limit,
			String afterItemId) throws DataStoreException
    {
		return null;
	}

	public int getNodeEntriesCount(String node) throws DataStoreException
	{
		return 0;
	}

	public boolean storeEntry(String nodename, String id, String entry) 
		throws DataStoreException
	{
		return false;
	}

	public String storeState(String oldID, String newID,
			Map<String, String> state) throws DataStoreException
    {
		return null;
	}

	public Map<String, String> getState(String id) throws DataStoreException
	{
		return null;
	}

	public boolean nodeExists(String createNodeId) throws DataStoreException
	{
		return false;
	}
}