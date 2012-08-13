package org.buddycloud.channelserver.db.jedis;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;


import org.bson.BSONObject;
import org.buddycloud.channelserver.pubsub.entry.NodeEntry;

import com.mongodb.DBObject;

public class NodeEntryImpl implements NodeEntry, DBObject {

    private HashMap<String, Object> obj = new HashMap<String, Object>();
    
    //public static final String KEY_
    
    public NodeEntryImpl() {
        
    }
    
    public NodeEntryImpl(String nodename, String id, String entry) {
        obj.put("id", id);
        obj.put("node", nodename);
        obj.put("entry", entry);
    }
    
    public String getId() {
        return (String) obj.get("id");
    }
    
    public String getMongoId() {
        //ObjectId o = new ObjectId();
        //o.toString()
        return (String) obj.get("_id").toString();
    }
    
    public String getEntry() {
        return (String) obj.get("entry");
    }
    
    public String getNode() {
        return (String) obj.get("node");
    }
    
    @Override
    public boolean containsField(String key) {
        return obj.containsKey(key);
    }

    @Override
    public boolean containsKey(String key) {
        return obj.containsKey(key);
    }

    @Override
    public Object get(String key) {
        return obj.get(key);
    }

    @Override
    public Set<String> keySet() {
        return obj.keySet();
    }

    @Override
    public Object put(String key, Object value) {
        return obj.put(key, value);
    }

    @Override
    public void putAll(BSONObject obj) {
        for (String k : obj.keySet()) {
            obj.put(k, obj.get(k));
        }
    }

    @Override
    public void putAll(Map map) {
        for (Object k : map.keySet()) {
            obj.put((String) k, map.get(k));
        }
    }

    @Override
    public Object removeField(String key) {
        return obj.remove(key);
    }

    @Override
    public Map<String, Object> toMap() {
        return (Map<String, Object>)obj;
    }

    @Override
    public boolean isPartialObject() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void markAsPartialObject() {
        // TODO Auto-generated method stub
        
    }

}
