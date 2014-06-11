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
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
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
		
		int maxItemsToReturn = MAX_ITEMS_TO_RETURN;
		String afterItemId   = null;

		String max_items = elm.attributeValue("max_items");
		if (max_items != null) {
			maxItemsToReturn = Integer.parseInt(max_items);
		}

		if (resultSetManagement != null) {
			Element max = resultSetManagement.element("max");
			if (max != null) {
				maxItemsToReturn = Integer.parseInt(max.getTextTrim());
			}
			Element after = resultSetManagement.element("after");
			if (after != null) {
				afterItemId = after.getTextTrim();
			}
		}
		boolean isProcessedLocally = true;

		if (node == null) {
			isProcessedLocally = getUserSubscriptions(subscriptions, maxItemsToReturn, afterItemId);
		} else {
			isProcessedLocally = getNodeSubscriptions(subscriptions, maxItemsToReturn, afterItemId);
		}
		if (false == isProcessedLocally) return;
		if ((resultSetManagement != null)
				|| (totalEntriesCount > maxItemsToReturn)) {
			/*
			 * TODO, add result set here as defined in 6.5.4 Returning Some
			 * Items <set xmlns='http://jabber.org/protocol/rsm'> <first
			 * index='0'>368866411b877c30064a5f62b917cffe</first>
			 * <last>4e30f35051b7b8b42abe083742187228</last> <count>19</count>
			 * </set>
			 */
			Element rsmElement = pubsub.addElement("set",
					"http://jabber.org/protocol/rsm");

			if (firstItem != null) {
				rsmElement.addElement("first").setText(firstItem);
				rsmElement.addElement("last").setText(lastItem);
			}
			rsmElement.addElement("count")
					.setText(Integer.toString(totalEntriesCount));
		}
		outQueue.put(result);
	}

	private boolean getNodeSubscriptions(Element subscriptions, int maxItemsToReturn, String afterItemId)
			throws NodeStoreException, InterruptedException {
		if (false == channelManager.isLocalNode(node) 
		    && (false == channelManager.isCachedNode(node))
		) {
			makeRemoteRequest(new JID(node.split("/")[2]).getDomain());
		    return false;
		}
		ResultSet<NodeSubscription> cur;
		
		if (null == afterItemId) {
		     cur = channelManager.getNodeSubscriptions(node, isOwnerModerator());
		} else {
			cur = channelManager.getNodeSubscriptions(node, isOwnerModerator(), new JID(afterItemId), maxItemsToReturn);
		}
		subscriptions.addAttribute("node", node);

		if ((null != requestIq.getElement().element("pubsub").element("set"))
				&& (0 == cur.size())
				&& (false == channelManager.isLocalNode(node))) {
			makeRemoteRequest(new JID(node.split("/")[2]).getDomain());
			return false;
		}
		
		for (NodeSubscription ns : cur) {

			if (null == firstItem) firstItem = ns.getUser().toBareJID();
			lastItem = ns.getUser().toBareJID();
			subscriptions
					.addElement("subscription")
					.addAttribute("node", ns.getNodeId())
					.addAttribute("subscription",
							ns.getSubscription().toString())
					.addAttribute("jid", ns.getUser().toBareJID());
		}
		totalEntriesCount = channelManager.countNodeSubscriptions(node, isOwnerModerator());
		return true;
	}

	private boolean isOwnerModerator() throws NodeStoreException {
		if (null == isOwnerModerator) {
			NodeAffiliation affiliation = channelManager.getUserAffiliation(node, actorJid);
			isOwnerModerator = affiliation.getAffiliation().in(Affiliations.moderator, Affiliations.owner);
		}
		return isOwnerModerator;
	}

	private boolean getUserSubscriptions(Element subscriptions, int maxItemsToReturn, String afterItemId)
			throws NodeStoreException, InterruptedException {
		if (false == channelManager.isLocalJID(actorJid) 
		    && (false == channelManager.isCachedJID(actorJid))
		) {
			makeRemoteRequest(actorJid.getDomain());
			return false;
		}
		// let's get all subscriptions.
		ResultSet<NodeSubscription> cur;
		if (null == afterItemId) {
		    cur = channelManager.getUserSubscriptions(actorJid);
		} else {
			cur = channelManager.getUserSubscriptions(actorJid, afterItemId, maxItemsToReturn);
		}

		if ((null != requestIq.getElement().element("pubsub").element("set"))
				&& (0 == cur.size())
				&& (false == channelManager.isLocalJID(actorJid))) {
			makeRemoteRequest(actorJid.getDomain());
			return false;
		}
		
		for (NodeSubscription ns : cur) {
			if (null == firstItem) firstItem = ns.getNodeId();
			lastItem = ns.getNodeId();
			subscriptions
					.addElement("subscription")
					.addAttribute("node", ns.getNodeId())
					.addAttribute("subscription",
							ns.getSubscription().toString())
					.addAttribute("jid", ns.getUser().toBareJID());
		}
		totalEntriesCount = channelManager.countUserSubscriptions(actorJid);
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