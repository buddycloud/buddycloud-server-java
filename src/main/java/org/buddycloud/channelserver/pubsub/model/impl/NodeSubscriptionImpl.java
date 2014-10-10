package org.buddycloud.channelserver.pubsub.model.impl;

import java.util.Date;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

public class NodeSubscriptionImpl implements NodeSubscription {

    private final Subscriptions subscription;
    private final JID user;
    private JID listener;
    private final String nodeId;
    private Date lastUpdated;
    private JID invitedBy;


    public NodeSubscriptionImpl(final String nodeId, final JID user, final Subscriptions subscription, JID invitedBy) {
        this(nodeId, user, user, subscription, invitedBy, new Date());
    }

    public NodeSubscriptionImpl(final String nodeId, final JID user, final Subscriptions subscription, JID invitedBy, Date lastUpdated) {
        this(nodeId, user, user, subscription, invitedBy, lastUpdated);
    }

    public NodeSubscriptionImpl(final String nodeId, final JID user, JID listener, final Subscriptions subscription, JID invitedBy) {
        this(nodeId, user, listener, subscription, invitedBy, new Date());
    }

    public NodeSubscriptionImpl(final String nodeId, final JID user, JID listener, final Subscriptions subscription, JID invitedBy, Date lastUpdated) {
        this.nodeId = nodeId;
        if (user.getResource() == null) {
            this.user = user;
        } else {
            this.user = new JID(user.toBareJID());
        }
        this.lastUpdated = new Date(lastUpdated.getTime());
        setListener(listener);
        this.subscription = subscription;
        this.invitedBy = invitedBy;
    }

    private void setListener(JID listener) {
        if (null == listener) {
            this.listener = this.user;
            return;
        }
        if (null == listener.getNode()) {
            this.listener = new JID(listener.getDomain());
            return;
        }
        this.listener = new JID(listener.toBareJID());
    }

    @Override
    public Subscriptions getSubscription() {
        return subscription;
    }

    @Override
    public JID getUser() {
        return user;
    }

    @Override
    public JID getListener() {
        return listener;
    }

    @Override
    public String getNodeId() {
        return nodeId;
    }

    @Override
    public JID getInvitedBy() {
        return invitedBy;
    }

    @Override
    public final int hashCode() {
        final int prime = 31;

        return new HashCodeBuilder(17, prime).append(listener).append(nodeId).append(subscription).append(user).append(invitedBy).toHashCode();
    }

    @Override
    public final boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (obj == this) {
            return true;
        }
        if (obj.getClass() != getClass()) {
            return false;
        }
        NodeSubscriptionImpl rhs = (NodeSubscriptionImpl) obj;
        return new EqualsBuilder().append(listener, rhs.listener).append(nodeId, rhs.nodeId).append(subscription, rhs.subscription).append(user, rhs.user)
                .append(invitedBy, rhs.invitedBy).isEquals();
    }

    @Override
    public String toString() {
        return "NodeSubscriptionImpl [subscription=" + subscription + ", user=" + user + ", listener=" + listener + ", nodeId=" + nodeId + "]";
    }

    @Override
    public String getUID() {
        return toString();
    }

    @Override
    public Date getLastUpdated() {
        // Return a defensive copy of the last updated time
        return new Date(lastUpdated.getTime());
    }
}
