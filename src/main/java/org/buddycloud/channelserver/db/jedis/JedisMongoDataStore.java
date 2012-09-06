package org.buddycloud.channelserver.db.jedis;

import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.bson.types.ObjectId;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.entry.NodeEntry;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;

import redis.clients.jedis.Jedis;

import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;
import com.mongodb.Mongo;
import com.mongodb.MongoException;
import com.mongodb.WriteConcern;

/**
 * This class is basically the access object to any data storage.
 * 
 *  THIS CLASS SHOULD BE REFACTORED.
 *  
 *  It should be build in a way that we could use any database as a backend.
 */

public class JedisMongoDataStore implements DataStore
{
    private Jedis jedis;
    private Mongo mongo;
    private DB mdb;
    
    private DBCollection subscriptions;
    private DBCollection entries;
    private DBCollection nodes;
    
    public  static final String LOCAL_USERS = "local_users";
    private static final Logger LOGGER      = Logger.getLogger(JedisMongoDataStore.class);
    
    public JedisMongoDataStore(Properties conf)
    {
        this.jedis = new Jedis(
        	conf.getProperty("redis.host"), 
            Integer.parseInt(conf.getProperty("redis.port"))
        );
        this.jedis.configSet("timeout", "0");
        
        try {
            this.mongo = new Mongo(
            	conf.getProperty("mongo.host"), 
                Integer.parseInt(conf.getProperty("mongo.port"))
            );
            this.mdb = this.mongo.getDB(conf.getProperty("mongo.db"));
            
            this.subscriptions = this.mdb.getCollection("subscriptions");
            this.subscriptions.setObjectClass(NodeSubscriptionImpl.class);
            DBObject indexes = new BasicDBObject();
            indexes.put("node", 1);
            indexes.put("bareJID", 1);
            this.subscriptions.ensureIndex(indexes, "unique_subscription", true);
            
            nodes = mdb.getCollection("nodes");
            indexes = new BasicDBObject();
            indexes.put("name", 1);
            nodes.ensureIndex(indexes, "unique_node", true);

            this.entries = this.mdb.getCollection("entries");
            this.entries.setObjectClass(NodeEntryImpl.class);
            indexes = new BasicDBObject();
            indexes.put("node", 1);
            this.entries.ensureIndex(indexes, "node_entries");
            
        } catch (NumberFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (MongoException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    public boolean isLocalNode(String nodename)
    {
    	DBObject query = new BasicDBObject();
        query.put("name", getNodeConfRedisKey(nodename));
        return (nodes.findOne(query) != null);
    }
    
    public Long addLocalUser(String bareJID)
    {
        return this.jedis.sadd(LOCAL_USERS, bareJID);
    }
    
    public boolean isLocalUser(String bareJID)
    {
        return this.jedis.sismember(LOCAL_USERS, bareJID);
    }
    
    public String addNodeConf(String nodename, Map<String, String> conf) 
    {
        BasicDBObject doc  = new BasicDBObject();
        BasicDBObject find = new BasicDBObject();

        doc.put("name", getNodeConfRedisKey(nodename));
        for (Map.Entry<String, String> entry : conf.entrySet()) {
        	doc.put(entry.getKey(), entry.getValue());
        }
        find.put("name", getNodeConfRedisKey(nodename));
        DBObject existingRecord = nodes.findOne(find);
        if (existingRecord != null) {
        	doc.put("pubsub#owner", existingRecord.get("pubsub#owner"));
        }
        nodes.remove(find);
        nodes.insert(doc);
        return "OK";
    }

    public String createUserNodes(String owner)
    {
        /*
           - /posts
           - /status
           - /subscriptions
           - /geo/previous
           - /geo/current
           - /geo/next
         */
        try {
        	if (false == nodeExists(Conf.getPostChannelNodename(owner))) {
		        this.createNode(
		            owner,
		            Conf.getPostChannelNodename(owner),
		            Conf.getDefaultPostChannelConf(owner)
		        );
        	}
	        if (false == nodeExists(Conf.getStatusChannelNodename(owner))) {
		        this.createNode(
		            owner,
		            Conf.getStatusChannelNodename(owner),
		            Conf.getDefaultStatusChannelConf(owner)
		        );
	        }
	        if (false == nodeExists(Conf.getSubscriptionsChannelNodename(owner))) {
		        this.createNode(
		        	owner,
		            Conf.getSubscriptionsChannelNodename(owner),
		            Conf.getDefaultSubscriptionsChannelConf(owner)
		        );
	        }
	        if (false == nodeExists(Conf.getGeoPreviousChannelNodename(owner))) {
		        this.createNode(
		        	owner,
		            Conf.getGeoPreviousChannelNodename(owner),
		            Conf.getDefaultGeoPreviousChannelConf(owner)
		        );
	        }
	        if (false == nodeExists(Conf.getGeoCurrentChannelNodename(owner))) {
		        this.createNode(
		        	owner,
		            Conf.getGeoCurrentChannelNodename(owner),
		            Conf.getDefaultGeoCurrentChannelConf(owner)
		        );
	        }
	        if (false == nodeExists(Conf.getGeoNextChannelNodename(owner))) {
		        this.createNode(
		        	owner,
		            Conf.getGeoNextChannelNodename(owner),
		            Conf.getDefaultGeoNextChannelConf(owner)
		        );
	        }
	        return "OK";
        } catch (DataStoreException e) {
        	return "FAIL";
        }
    }
    
    public void createNode(String owner, String nodename, Map<String, String> conf) 
        throws DataStoreException
    {
        this.addNodeConf(nodename, conf);
        
        this.subscribeUserToNode(
            owner, 
	        nodename, 
	        Affiliations.owner.toString(), 
	        Subscriptions.subscribed.toString(),
	        null
	    );
    }
    
    public boolean subscribeUserToNode(String bareJID, String nodename, 
        String aff, String subs, String foreignChannelServer
    ) {
    	try {
            this.subscriptions.save(
                new NodeSubscriptionImpl(bareJID, nodename, aff, subs, foreignChannelServer),
                WriteConcern.SAFE
            );
    	} catch (MongoException e) {
    		e.printStackTrace();
    	}
        return true;
    }
    
    public boolean unsubscribeUserFromNode(String bareJID, String node) {
        
        DBObject query = new BasicDBObject();
        query.put("node", node);
        query.put("bareJID", bareJID);
        
        this.subscriptions.remove(query, WriteConcern.SAFE);
        return true;
    }

    @SuppressWarnings("unchecked")
    public Iterator<? extends NodeSubscription> getUserSubscriptionsOfNodes(String bareJID) {
        
        DBObject query = new BasicDBObject();
        query.put("bareJID", bareJID);
        return new CastingIterator<DBObject, NodeSubscription>(this.subscriptions.find(query).iterator());
    }
    
    public NodeSubscriptionImpl getUserSubscriptionOfNode(String bareJID, String node)
    {        
        DBObject query = new BasicDBObject();
        query.put("node", node);
        query.put("bareJID", bareJID);
        
        LOGGER.trace("Getting subscription of node " + node + " for " + bareJID);
        NodeSubscriptionImpl sub = (NodeSubscriptionImpl) this.subscriptions.findOne(query);
        if(sub == null) {
            return new NodeSubscriptionImpl();
        }
        return sub;
    }

    @SuppressWarnings("unchecked")
    public Iterator<? extends NodeSubscription> getNodeSubscribers(String node)
    {
        DBObject query = new BasicDBObject();
        query.put("node", node);
        LOGGER.trace(
            "Node subscribers list for node " + node + ", there are " 
            + this.subscriptions.find(query).size() + " results"
        );
        return new CastingIterator<DBObject, NodeSubscription>(this.subscriptions.find(query).iterator());
    }
    
    public HashMap<String, String> getNodeConf(String nodename)
    {
        //return (HashMap<String, String>) this.hgetAll(getNodeConfRedisKey(nodename));
    	DBObject query = new BasicDBObject();
    	query.put("name", getNodeConfRedisKey(nodename));
    	DBObject record = nodes.findOne(query);
    	HashMap<String, String> conf = new HashMap<String, String>();
    	HashMap<String, Object> recordMap = (HashMap<String, Object>) record;
    	for (Map.Entry<String, Object> entry : recordMap.entrySet()) {
    		if (entry.getValue() instanceof String) {
    			conf.put(entry.getKey(), entry.getValue().toString());
    		}
    	}
    	return conf;
    }

    /*
     
        Sort by _id to sort by insertion time
        BSON ObjectId's begin with a timestamp. Thus sorting by _id, when using the ObjectID type, 
        results in sorting by time. Note: granularity of the timestamp portion of the ObjectID is to one second only.
    
        > // get 10 newest items
        > db.mycollection.find().sort({id:-1}).limit(10); 
         
     */
    
    // Entry fetching related
    
    @SuppressWarnings("unchecked")
    public Iterator<? extends NodeEntry> getNodeEntries(String node, int limit, String afterItemId)
    {
        DBObject query = new BasicDBObject();
        query.put("node", node);
        
        if(afterItemId != null) {
            query.put("_id", new BasicDBObject("$lt", new ObjectId(afterItemId)));
        }
        
        DBObject sort = new BasicDBObject();
        sort.put("_id", -1);
        
        return new CastingIterator<DBObject, NodeEntry>(this.entries.find(query).sort(sort).limit(limit).iterator());
    }
    
    public int getNodeEntriesCount(String node)
    {        
        DBObject query = new BasicDBObject();
        query.put("node", node);
        return this.entries.find(query).count();
    }
    
    //
    
    // Publishing related
    
    public boolean storeEntry(String nodename, String id, String entry)
    {
        this.entries.save(new NodeEntryImpl(nodename, id, entry), WriteConcern.SAFE);
        return true;
    }
    
    // End of publishing related
    
    // TODO, this statemachine stuff could go to other place too
    public String storeState(String oldID, String newID, Map<String, String> state)
    {
        this.jedis.del("state:" + oldID);
        
        if(state.isEmpty()) {
            return "OK";
        }
        
        return this.jedis.hmset("state:" + newID, state);
    }
    
    public Map<String, String> getState(String id)
    {
        return (Map<String, String>) this.jedis.hgetAll("state:" + id);
    }
    
    // TODO, move these to somewhere else i think...
    public static String getNodeConfRedisKey(String nodename)
    {
        return "node:" + nodename + ":conf";
    }

	@Override
	public boolean nodeExists(String createNodeId)
	{
		return isLocalNode(createNodeId);
	}
	
	/**
	 * Find subscribed nodes for a particular listener
	 */
	public Iterator<? extends NodeSubscription> findUserSubscriptionOfNodes(
	    String listener, String nodeMatch)
	{
		Pattern pattern = Pattern.compile("^/user/" + nodeMatch + "/.*$");
        DBObject query  = new BasicDBObject();
        query.put("bareJID", listener);
        query.put("node", pattern);

        return new CastingIterator<DBObject, NodeSubscription>(this.subscriptions.find(query).iterator());
	}
}