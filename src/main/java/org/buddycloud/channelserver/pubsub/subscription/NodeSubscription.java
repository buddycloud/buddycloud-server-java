package org.buddycloud.channelserver.pubsub.subscription;

import org.xmpp.packet.JID;

public interface NodeSubscription {

    Subscriptions getSubscription();
    
    JID getUser();
    
    String getNodeID();
    
    String getForeignChannelServer();
    
}
