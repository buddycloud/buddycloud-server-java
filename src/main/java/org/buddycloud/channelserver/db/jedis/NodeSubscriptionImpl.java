package org.buddycloud.channelserver.db.jedis;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;

import org.bson.BSONObject;

import com.mongodb.DBObject;

public class NodeSubscriptionImpl implements NodeSubscription, DBObject
{
    private HashMap<String, Object> sub = new HashMap<String, Object>();
    
    //public static final String KEY_
    
    public NodeSubscriptionImpl()
    {
        
    }
    
    public NodeSubscriptionImpl(String bareJID, String node, String aff, 
        String subs, String foreignChannelServer
    ) {
        sub.put("node", node);
        sub.put("bareJID", bareJID);
        sub.put("affiliation", aff);
        sub.put("subscription", subs);
        sub.put("foreignChannelServer", foreignChannelServer);
    }

    public String getAffiliation()
    {
        return (String) sub.get("affiliation"); 
    }

    public String getSubscription()
    {
        return (String) sub.get("subscription"); 
    }

    public String getBareJID()
    {
        return (String) sub.get("bareJID"); 
    }

    public String getNode()
    {
        return (String) sub.get("node"); 
    }

    public String getForeignChannelServer()
    {
        return (String) sub.get("foreignChannelServer"); 
    }

    public boolean containsField(String key)
    {
        return sub.containsKey(key);
    }

    public boolean containsKey(String key)
    {
        return sub.containsKey(key);
    }

    public Object get(String key)
    {
        return sub.get(key);
    }

    public Set<String> keySet()
    {
        return sub.keySet();
    }

    public Object put(String key, Object value)
    {
        return sub.put(key, value);
    }

    public void putAll(BSONObject obj)
    {
        for (String k : obj.keySet()) {
            sub.put(k, obj.get(k));
        }
    }

    @Override
    public void putAll(Map map)
    {
        for (Object k : map.keySet()) {
            sub.put((String) k, map.get(k));
        }
    }

    public Object removeField(String key)
    {
        return sub.remove(key);
    }

    public Map<String, Object> toMap()
    {
        return (Map<String, Object>)sub;
    }

    public boolean isPartialObject()
    {
        // TODO Auto-generated method stub
        return false;
    }

    public void markAsPartialObject()
    {
        // TODO Auto-generated method stub
    }
}