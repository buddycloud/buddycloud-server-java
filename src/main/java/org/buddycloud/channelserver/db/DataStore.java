package org.buddycloud.channelserver.db;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.buddycloud.channelserver.pubsub.entry.NodeEntry;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;



public interface DataStore {

    public boolean isLocalNode(String nodename) throws DataStoreException;
    
    public Long addLocalUser(String bareJID) throws DataStoreException;
    
    public boolean isLocalUser(String bareJID) throws DataStoreException;
    
    public String addNodeConf(String nodename, Map<String, String> conf) throws DataStoreException;
    
    public String createUserNodes(String owner) throws DataStoreException;
    
    public void createNode(String owner, String nodename, 
            Map<String, String> conf) throws DataStoreException;
    
    public boolean subscribeUserToNode(String bareJID, String nodename, 
            String aff, String subs, String foreignChannelServer) throws DataStoreException;
    
    public boolean unsubscribeUserFromNode(String bareJID, String node) throws DataStoreException;

    public Iterator<? extends NodeSubscription> getUserSubscriptionsOfNodes(String bareJID) throws DataStoreException;
    
    public NodeSubscriptionImpl getUserSubscriptionOfNode(String bareJID, String node) throws DataStoreException;

    public Iterator<? extends NodeSubscription> getNodeSubscribers(String node) throws DataStoreException;
    
    public HashMap<String, String> getNodeConf(String nodename) throws DataStoreException;
    
    public Iterator<? extends NodeEntry> getNodeEntries(String node, 
            int limit, String afterItemId) throws DataStoreException;
    
    public int getNodeEntriesCount(String node) throws DataStoreException;
    
    public boolean storeEntry(String nodename, String id, String entry) throws DataStoreException;
    
    public String storeState(String oldID, String newID, Map<String, String> state) throws DataStoreException;
    
    public Map<String, String> getState(String id) throws DataStoreException;

	public boolean nodeExists(String createNodeId) throws DataStoreException;
    
}
