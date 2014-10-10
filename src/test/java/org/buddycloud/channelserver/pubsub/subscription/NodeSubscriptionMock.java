package org.buddycloud.channelserver.pubsub.subscription;

import java.util.Date;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.xmpp.packet.JID;

public class NodeSubscriptionMock implements NodeSubscription {
    private JID subscriber;
    private Subscriptions subscription;

    public void setBareJID(JID jid) {
        this.subscriber = jid;
    }

    public void setAffiliation(Affiliations affiliation) {}

    public void setSubscription(Subscriptions subscription) {
        this.subscription = subscription;
    }

    public void setForeignChannelServer(String foreignChannelServer) {}

    public NodeSubscriptionMock(JID jid) {
        subscriber = jid;
    }

    @Override
    public Subscriptions getSubscription() {
        return subscription;
    }

    @Override
    public JID getUser() {
        return subscriber;
    }

    @Override
    public JID getListener() {
        return subscriber;
    }

    @Override
    public String getNodeId() {
        return null;
    }

    @Override
    public String getUID() {
        return subscriber.toString();
    }

    @Override
    public Date getLastUpdated() {
        return new Date();
    }

    @Override
    public JID getInvitedBy() {
        return null;
    }
}
