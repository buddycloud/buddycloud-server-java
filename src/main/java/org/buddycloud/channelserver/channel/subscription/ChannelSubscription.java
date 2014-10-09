package org.buddycloud.channelserver.channel.subscription;

import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

public class ChannelSubscription {
    private final JID user;
    private final JID channel;
    private final Subscriptions type;

    public ChannelSubscription(final JID user, final JID channel, final Subscriptions type) {
        this.user = user;
        this.channel = channel;
        this.type = type;
    }

    public JID getUserJID() {
        return null;
    }

    public JID getChannelJID() {
        return null;
    }

    public Subscriptions getType() {
        return null;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((channel == null) ? 0 : channel.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        result = prime * result + ((user == null) ? 0 : user.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ChannelSubscription other = (ChannelSubscription) obj;
        if (channel == null) {
            if (other.channel != null) {
                return false;
            }
        } else if (!channel.equals(other.channel)) {
            return false;
        }
        if (type != other.type) {
            return false;
        }
        if (user == null) {
            if (other.user != null) {
                return false;
            }
        } else if (!user.equals(other.user)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "ChannelSubscription [user=" + user + ", channel=" + channel + ", type=" + type + "]";
    }
}
