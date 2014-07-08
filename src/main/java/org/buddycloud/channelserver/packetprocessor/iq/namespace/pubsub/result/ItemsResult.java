package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.util.Date;
import java.util.List;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class ItemsResult extends PubSubElementProcessorAbstract {

	private static final String MISSING_NODE = "Missing node";
	private static final Logger logger = Logger.getLogger(ItemsResult.class);
	private boolean subscriptionNode = false;

	public ItemsResult(ChannelManager channelManager) {
		this.channelManager = channelManager;
	}

	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {

		this.request = reqIQ;
		
		if (-1 != request.getFrom().toString().indexOf("@")) {
			logger.debug("Ignoring result packet, only interested in stanzas "
					+ "from other buddycloud servers");
			return;
		}

		if (null != elm.attributeValue("node")) 
			this.setNode(elm.attributeValue("node"));

		if ((null == node) || (true == node.equals(""))) {
			throw new NullPointerException(MISSING_NODE);
		}

		subscriptionNode = (true == node.substring(node.length() - 13,
				node.length()).equals("subscriptions"));

		if ((false == subscriptionNode)
				&& (false == channelManager.nodeExists(node)))
			channelManager.addRemoteNode(node);

		@SuppressWarnings("unchecked")
		List<Element> items = request.getElement().element("pubsub")
				.element("items").elements("item");

		for (Element item : items) {
			if (true == subscriptionNode) {
				processSubscriptionItem(item);
			} else {
				processPublishedItem(item);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void processSubscriptionItem(Element item)
			throws NodeStoreException {
		JID user = new JID(item.attributeValue("id"));
		List<Element> items = item.element("query").elements("item");
		for (Element subscription : items) {
			try {
			    addSubscription(subscription, user);
			} catch (IllegalArgumentException e) {
				logger.error(e);
			}
		}
	}

	private void addSubscription(Element item, JID user)
			throws NodeStoreException {

		String node = item.attributeValue("node");
		
		// If its a local JID and/or a local node, that's our turf!
		if ((true == channelManager.isLocalNode(node))
				&& (true == channelManager.isLocalJID(user)))
			return;
        if (true == channelManager.isLocalNode(node)) return; 
        
		JID listener = request.getFrom();
		Subscriptions sub = Subscriptions.createFromString(item
				.attributeValue("subscription"));
		Affiliations aff = Affiliations.createFromString(item
				.attributeValue("affiliation"));
		NodeSubscription subscription = new NodeSubscriptionImpl(node, user,
				listener, sub, null);

		if (false == channelManager.nodeExists(node))
			channelManager.addRemoteNode(node);

		channelManager.addUserSubscription(subscription);
		channelManager.setUserAffiliation(node, user, aff);
	}

	private void processPublishedItem(Element item) throws NodeStoreException {

		Element entry = item.element("entry");

		try {
			// Probably a tombstone'd item
			if (null == entry.elementText("updated")) {
				logger.debug("Entry has no 'updated' element, won't process");
				return;
			}
			String inReplyTo = null;
			Element reply;
			
			if (null != (reply = entry.element("in-reply-to"))) {
				String[] inReplyToParts = reply.attributeValue("ref").split(",");
				inReplyTo = inReplyToParts[inReplyToParts.length - 1];
			}
			Date updatedDate = Conf.parseDate(entry.elementText("updated"));
			NodeItemImpl nodeItem = new NodeItemImpl(node,
					GlobalItemIDImpl.toLocalId(entry.elementText("id")), 
					updatedDate, entry.asXML(), inReplyTo);
			try {
				channelManager
						.deleteNodeItemById(node, entry.elementText("id"));
			} catch (NodeStoreException e) {
				logger.error("Attempt to delete an item which didn't exist... its ok");
			}
			channelManager.addNodeItem(nodeItem);
		} catch (IllegalArgumentException e) {
			logger.error(e);
			e.printStackTrace();
			return;
		}
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}
}