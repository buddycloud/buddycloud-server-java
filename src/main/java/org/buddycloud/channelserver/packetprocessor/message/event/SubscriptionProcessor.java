package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class SubscriptionProcessor extends AbstractMessageProcessor {

	private JID jid;
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

		if (false == channelManager.isLocalNode(node)) {
			sendLocalNotifications();
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

		if (true == channelManager.isLocalNode(node)) {
			return;
		}
		storeNewSubscription();
	}

	private void storeNewSubscription() throws NodeStoreException {
		NodeSubscriptionImpl newSubscription = new NodeSubscriptionImpl(node,
				jid, subscription);
		addRemoteNode();
		channelManager.addUserSubscription(newSubscription);
	}

	private void addRemoteNode() {
        try { 
            if (false == channelManager.nodeExists(node))
                channelManager.addRemoteNode(node); 
        } catch (NodeStoreException e) { 
        	logger.error(e);
        }
	}
}