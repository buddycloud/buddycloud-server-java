package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Collection;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.message.MessageProcessor;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.dom4j.Element;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscriptionProcessor implements PacketProcessor<Message> {

	private ChannelManager channelManager;
	private JID jid;
	private Affiliations affiliation;
	private Subscriptions subscription;
	private String node;
	private Properties configuration;
	private BlockingQueue<Packet> outQueue;
	private Message message;

	private static final Logger logger = Logger
			.getLogger(SubscriptionProcessor.class);

	public SubscriptionProcessor(BlockingQueue<Packet> outQueue,
			Properties configuration, ChannelManager channelManager) {
		this.channelManager = channelManager;
		this.configuration = configuration;
		this.outQueue = outQueue;
	}

	@Override
	public void process(Message packet) throws Exception {
		message = packet;

		handleSubscriptionElement();
		handleAffiliationElement();

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
		if (true == channelManager.isLocalJID(jid)) {
			storeNewSubscription();
		}
	}

	private void handleAffiliationElement() throws NodeStoreException {
		Element affiliationElement = message.getElement().element("event")
				.element("affiliation");
		if (null == affiliationElement) {
			return;
		}
		jid = new JID(affiliationElement.attributeValue("jid"));
		node = affiliationElement.attributeValue("node");
		affiliation = Affiliations.valueOf(affiliationElement
				.attributeValue("affiliation"));
		if (true == channelManager.isLocalNode(node)) {
			return;
		}
		if (true == channelManager.isLocalJID(jid)) {
			storeNewAffiliation();
		}
	}

	private void sendLocalNotifications() throws Exception {
		ResultSet<NodeSubscription> subscribers = channelManager
				.getNodeSubscriptions(node);
		for (NodeSubscription subscriber : subscribers) {
			message.setTo(subscriber.getUser());
			message.setFrom(new JID(configuration
					.getProperty("server.domain.channels")));
			outQueue.put(message.createCopy());
		}
	}

	private void storeNewSubscription() throws NodeStoreException {
		NodeSubscriptionImpl newSubscription = new NodeSubscriptionImpl(node,
				jid, subscription);
		addRemoteNode();
		channelManager.addUserSubscription(newSubscription);
	}

	private void storeNewAffiliation() throws NodeStoreException {
		addRemoteNode();
		channelManager.setUserAffiliation(node, jid, affiliation);
	}

	private void addRemoteNode() {
        try { 
            channelManager.addRemoteNode(node); 
        } catch (NodeStoreException e) { 
        	logger.error(e);
        }
	}
}