package org.buddycloud.channelserver.pubsub.subscription;

public interface NodeSubscription {

    public String getAffiliation();
    
    public String getSubscription();
    
    public String getBareJID();
    
    public String getNode();
    
    public String getForeignChannelServer();
    
}
