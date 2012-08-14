package org.buddycloud.channelserver.db.mock;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.entry.NodeEntry;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;


public class Mock implements DataStore
{
	@Override
	public boolean isLocalNode(String nodename)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Long addLocalUser(String bareJID)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isLocalUser(String bareJID)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String addNodeConf(String nodename, HashMap<String, String> conf)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String createUserNodes(String owner)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String createNode(String owner, String nodename,
			HashMap<String, String> conf)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean subscribeUserToNode(String bareJID, String nodename,
			String aff, String subs, String foreignChannelServer)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean unsubscribeUserFromNode(String bareJID, String node)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Iterator<? extends NodeSubscription> getUserSubscriptionsOfNodes(
			String bareJID)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSubscriptionImpl getUserSubscriptionOfNode(String bareJID,
			String node)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterator<? extends NodeSubscription> getNodeSubscribers(String node)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public HashMap<String, String> getNodeConf(String nodename)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterator<? extends NodeEntry> getNodeEntries(String node, int limit,
			String afterItemId)
    {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getNodeEntriesCount(String node)
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean storeEntry(String nodename, String id, String entry)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String storeState(String oldID, String newID,
			Map<String, String> state)
    {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, String> getState(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}
}