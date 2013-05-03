package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class ItemsResult extends PubSubElementProcessorAbstract {

	private static final String MISSING_NODE = "Missing node";
	private static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.S'Z'";

	private static final Logger logger = Logger.getLogger(ItemsResult.class);
	private SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
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

		this.setNode(elm.attributeValue("node"));

		if ((null == node) || (true == node.equals(""))) {
			throw new NullPointerException(MISSING_NODE);
		}

		subscriptionNode = (true == node.substring(
				node.length() - 13, node.length()).equals("subscriptions"));

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

	private void processSubscriptionItem(Element item) throws NodeStoreException {
		JID user = new JID(item.attributeValue("id"));
		List<Element> items = item.element("query").elements("item");
		for (Element subscription : items) {
			addSubscription(subscription, user);
		}
	}

	private void addSubscription(Element item, JID user) throws NodeStoreException {
		
		
		String node       = item.attributeValue("node");
		JID    listener   = request.getFrom();
		Subscriptions sub = Subscriptions.createFromString(item.attributeValue("subscription"));
		Affiliations aff  = Affiliations.createFromString(item.attributeValue("affiliation"));
		NodeSubscription subscription = new NodeSubscriptionImpl(node, user, listener, sub);
		
		if (false == channelManager.nodeExists(node)) 
			channelManager.addRemoteNode(node);
		
		channelManager.addUserSubscription(subscription);
		channelManager.setUserAffiliation(node, user, aff);
	}

	private void processPublishedItem(Element item) throws ParseException,
			NodeStoreException {

		Element entry = item.element("entry");

		try {
			// Probably a tombstone'd item
			if (null == entry.elementText("updated")) return;
			
			Date updatedDate = sdf.parse(entry.elementText("updated"));
			NodeItemImpl nodeItem = new NodeItemImpl(node,
					entry.elementText("id"), updatedDate, entry.asXML());
			try {
			    channelManager.deleteNodeItemById(node, entry.elementText("id"));
			} catch (NodeStoreException e) {
				logger.error("Attempt to delete an item which didn't exist... its ok");
			}
			channelManager.addNodeItem(nodeItem);
		} catch (ParseException e) {
			logger.error(e);
			return;
		}
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}
}