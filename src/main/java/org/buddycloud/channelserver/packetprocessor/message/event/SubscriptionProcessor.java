package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.NotificationScheme;
import org.dom4j.Element;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class SubscriptionProcessor extends AbstractMessageProcessor {

    private JID jid;
    private JID invitedBy = null;
    private Subscriptions subscription;

    private static final Logger logger = Logger
            .getLogger(SubscriptionProcessor.class);

    public SubscriptionProcessor(BlockingQueue<Packet> outQueue,
            Properties configuration, ChannelManager channelManager) {
        super(channelManager, configuration, outQueue);
    }

    @Override
    public void process(Message packet) throws Exception {
        message = packet;

        handleSubscriptionElement();

        if (null == subscription) {
            return;
        }
        if (true == Configuration.getInstance().isLocalNode(node)) {
            return;
        }
        if (subscription.equals(Subscriptions.pending)) {
            sendLocalNotifications(NotificationScheme.ownerOrModerator, jid);
        } else if (subscription.equals(Subscriptions.invited)) {
            sendLocalNotifications(NotificationScheme.ownerOrModerator, jid);
        } else if (subscription.equals(Subscriptions.subscribed)) {
            sendLocalNotifications(NotificationScheme.validSubscribers, null);
        } else if (subscription.equals(Subscriptions.none)) {
            sendLocalNotifications(NotificationScheme.validSubscribers, jid);
        }
    }

    private void handleSubscriptionElement() throws NodeStoreException {
        Element subscriptionElement = message.getElement().element("event")
                .element("subscription");
        if (null == subscriptionElement) {
            return;
        }

        jid = new JID(subscriptionElement.attributeValue("jid"));
        node = subscriptionElement.attributeValue("node");
        subscription = Subscriptions.valueOf(subscriptionElement
                .attributeValue("subscription"));
        if (null != subscriptionElement.attributeValue("invited-by")) {
            invitedBy = new JID(subscriptionElement.attributeValue("invited-by"));
        }

        if (true == Configuration.getInstance().isLocalNode(node)) {
            return;
        }
        storeNewSubscription();
    }

    private void storeNewSubscription() throws NodeStoreException {
        NodeSubscriptionImpl newSubscription = new NodeSubscriptionImpl(node,
                jid, subscription, invitedBy);
        addRemoteNode();
        channelManager.addUserSubscription(newSubscription);
    }

    private void addRemoteNode() {
        try { 
            if (false == channelManager.nodeExists(node)) {
                channelManager.addRemoteNode(node); 
            }
        } catch (NodeStoreException e) { 
            logger.error(e);
        }
    }
}
