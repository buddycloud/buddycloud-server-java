package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class SubscriptionsGet implements PubSubElementProcessor {

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;

	private static final Logger LOGGER = Logger
			.getLogger(SubscriptionsGet.class);

	public SubscriptionsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		IQ result = IQ.createResultIQ(reqIQ);
		Element pubsub = result.setChildElement(PubSubGet.ELEMENT_NAME,
				elm.getNamespaceURI());
		Element subscriptions = pubsub.addElement("subscriptions");

		String node = elm.attributeValue("node");

		if (actorJID == null) {
			actorJID = reqIQ.getFrom();
		}

		if (node == null) {
			// let's get all subscriptions.
			Collection<NodeSubscription> cur = channelManager
					.getUserSubscriptions(actorJID);

			for (NodeSubscription ns : cur) {
				subscriptions
						.addElement("subscription")
						.addAttribute("node", ns.getNodeId())
						.addAttribute("subscription",
								ns.getSubscription().toString())
						.addAttribute("jid", ns.getUser().toBareJID());
			}

		} else {
			Collection<NodeSubscription> cur = channelManager
					.getNodeSubscriptions(node);
			subscriptions.addAttribute("node", node);

			for (NodeSubscription ns : cur) {
				subscriptions
						.addElement("subscription")
						.addAttribute("node", ns.getNodeId())
						.addAttribute("subscription",
								ns.getSubscription().toString())
						.addAttribute("jid", ns.getUser().toBareJID());
			}
		}
		outQueue.put(result);
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("subscriptions");
	}
}
