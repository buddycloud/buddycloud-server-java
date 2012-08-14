package org.buddycloud.channelserver.db;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.buddycloud.channelserver.pubsub.entry.NodeEntry;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;

import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;



public interface DataStore {

    public boolean isLocalNode(String nodename);
    
    public Long addLocalUser(String bareJID);
    
    public boolean isLocalUser(String bareJID);
    
    public String addNodeConf(String nodename, HashMap<String, String> conf);
    
    public String createUserNodes(String owner);
    
    public String createNode(String owner, String nodename, 
            HashMap<String, String> conf);
    
    public boolean subscribeUserToNode(String bareJID, String nodename, 
            String aff, String subs, String foreignChannelServer);
    
    public boolean unsubscribeUserFromNode(String bareJID, String node);

    public Iterator<? extends NodeSubscription> getUserSubscriptionsOfNodes(String bareJID);
    
    public NodeSubscriptionImpl getUserSubscriptionOfNode(String bareJID, String node);

    public Iterator<? extends NodeSubscription> getNodeSubscribers(String node);
    
    public HashMap<String, String> getNodeConf(String nodename);
    
    public Iterator<? extends NodeEntry> getNodeEntries(String node, 
            int limit, String afterItemId);
    
    public int getNodeEntriesCount(String node);
    
    public boolean storeEntry(String nodename, String id, String entry);
    
    public String storeState(String oldID, String newID, Map<String, String> state);
    
    public Map<String, String> getState(String id);
    
}
