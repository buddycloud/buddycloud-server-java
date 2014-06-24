package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscriptionsGet implements PubSubElementProcessor {

	private final BlockingQueue<Packet> outQueue;
	private ChannelManager channelManager;
	
	private IQ result;
	private String node;
	private JID actorJid;
	private IQ requestIq;
	private Element resultSetManagement;
	
	private String firstItem;
	private String lastItem;
	private int totalEntriesCount;
	private Boolean isOwnerModerator;;

	private static final Logger logger = Logger
			.getLogger(SubscriptionsGet.class);

	public void setChannelManager(ChannelManager dataStore) {
		channelManager = dataStore;
	}
	
	public SubscriptionsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		result = IQ.createResultIQ(reqIQ);
		actorJid = actorJID;
		requestIq = reqIQ;
		resultSetManagement = rsm;
		
		if (false == channelManager.isLocalJID(requestIq.getFrom())) {
        	result.getElement().addAttribute("remote-server-discover", "false");
        }
		Element pubsub = result.setChildElement(PubSubGet.ELEMENT_NAME,
				elm.getNamespaceURI());
		Element subscriptions = pubsub.addElement("subscriptions");

		node = elm.attributeValue("node");

		if (null == actorJid) {
			actorJid = reqIQ.getFrom();
		}
		
		boolean isProcessedLocally = true;

		if (node == null) {
			isProcessedLocally = getUserMemberships(subscriptions);
		} else {
			isProcessedLocally = getNodeMemberships(subscriptions);
		}
		if (false == isProcessedLocally) return;
		
		outQueue.put(result);
	}

	private boolean getNodeMemberships(Element subscriptions)
			throws NodeStoreException, InterruptedException {
		if (false == channelManager.isLocalNode(node) 
		    && (false == channelManager.isCachedNode(node))
		) {
			makeRemoteRequest(new JID(node.split("/")[2]).getDomain());
		    return false;
		}
		ResultSet<NodeMembership> cur;
		cur = channelManager.getNodeMemberships(node);

		subscriptions.addAttribute("node", node);

		if ((null != requestIq.getElement().element("pubsub").element("set"))
				&& (0 == cur.size())
				&& (false == channelManager.isLocalNode(node))) {
			makeRemoteRequest(new JID(node.split("/")[2]).getDomain());
			return false;
		}
		boolean isOwnerModerator = isOwnerModerator();
		for (NodeMembership ns : cur) {
			if (false == actorJid.toBareJID().equals(ns.getUser())) {
				if ((false == isOwnerModerator) && ns.getAffiliation().in(Affiliations.outcast, Affiliations.none)) {
					continue;
				}
				if ((false == isOwnerModerator) && !ns.getSubscription().equals(Subscriptions.subscribed)) {
					continue;
				}
			}
			if (null == firstItem) firstItem = ns.getUser().toBareJID();
			lastItem = ns.getUser().toBareJID();
			subscriptions
					.addElement("subscription")
					.addAttribute("node", ns.getNodeId())
					.addAttribute("subscription",
							ns.getSubscription().toString())
					.addAttribute("jid", ns.getUser().toBareJID());
		}
		return true;
	}

	private boolean isOwnerModerator() throws NodeStoreException {
		return channelManager.getNodeMembership(node, actorJid).getAffiliation().canAuthorize();
	}

	private boolean getUserMemberships(Element subscriptions)
			throws NodeStoreException, InterruptedException {
		if (false == channelManager.isLocalJID(actorJid) 
		    && (false == channelManager.isCachedJID(actorJid))
		) {
			makeRemoteRequest(actorJid.getDomain());
			return false;
		}
		// let's get all subscriptions.
		ResultSet<NodeMembership> cur;
		cur = channelManager.getUserMemberships(actorJid);

		if ((null != requestIq.getElement().element("pubsub").element("set"))
				&& (0 == cur.size())
				&& (false == channelManager.isLocalJID(actorJid))) {
			makeRemoteRequest(actorJid.getDomain());
			return false;
		}
		boolean isOwnerModerator = isOwnerModerator();
		for (NodeMembership ns : cur) {
			if (false == actorJid.toBareJID().equals(ns.getUser())) {
				if ((false == isOwnerModerator) && ns.getAffiliation().in(Affiliations.outcast, Affiliations.none)) {
					continue;
				}
				if ((false == isOwnerModerator) && !ns.getSubscription().equals(Subscriptions.subscribed)) {
					continue;
				}
			}
			subscriptions
					.addElement("subscription")
					.addAttribute("node", ns.getNodeId())
					.addAttribute("subscription",
							ns.getSubscription().toString())
					.addAttribute("jid", ns.getUser().toBareJID());
		}
		return true;
	}
	
	private void makeRemoteRequest(String to) throws InterruptedException {
		requestIq.setTo(to);
		if (null == requestIq.getElement().element("pubsub").element("actor")) {
		    Element actor = requestIq.getElement().element("pubsub")
				.addElement("actor", Buddycloud.NS);
		    actor.addText(requestIq.getFrom().toBareJID());
		}
	    outQueue.put(requestIq);
	}
	
	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("subscriptions");
	}
}