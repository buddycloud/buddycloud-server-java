package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.Collection;
import java.util.Date;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ValidateEntry;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubSet;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class Publish extends PubSubElementProcessorAbstract {

	private static final Logger logger = Logger.getLogger(Publish.class);

	public static final String MISSING_ITEM_ELEMENT = "item-required";
	public static final String NODE_ID_REQUIRED = "nodeid-required";

	private Element entry;
	private String id;
	private JID publishersJID;
	private String inReplyTo;
	private Element item;
	private ValidateEntry entryContent;

	private ValidateEntry validator;

	public Publish(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {

		request = reqIQ;
		response = IQ.createResultIQ(reqIQ);
		publishersJID = request.getFrom();

		node = request.getChildElement().element("publish")
				.attributeValue("node");

		if (false == checkNode())
			return;

		boolean isLocalSubscriber = false;

		if (actorJID != null) {
			publishersJID = actorJID;
		} else {

			isLocalSubscriber = channelManager.isLocalJID(publishersJID);

			// Check that user is registered.
			if (!isLocalSubscriber) {

				// If the packet did not have actor, and the sender is not a
				// local user.
				// publishing is not allowed.

				/*
				 * <iq type='error' from='pubsub.shakespeare.lit'
				 * to='hamlet@denmark.lit/elsinore' id='create1'> <error
				 * type='auth'> <registration-required
				 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> </error> </iq>
				 */
				response.setType(Type.error);
				PacketError pe = new PacketError(
						org.xmpp.packet.PacketError.Condition.registration_required,
						org.xmpp.packet.PacketError.Type.auth);
				response.setError(pe);
				outQueue.put(response);
				return;
			}
		}

		try {
			if (false == nodeExists())
				return;
			if (false == userCanPost())
				return;
			if (false == isRequestValid())
				return;
			extractItemDetails();
			saveNodeItem();
			sendResponseStanza();
			sendNotifications();
			
		} catch (NodeStoreException e) {
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
			outQueue.put(response);
		}

	}

	private void saveNodeItem() throws NodeStoreException {
		// Let's store the new item.
		channelManager.addNodeItem(new NodeItemImpl(node, id, new Date(), entry
				.asXML(), inReplyTo));
	}

	private void extractItemDetails() throws InterruptedException {

		entry = entryContent.getPayload();
		id = GlobalItemIDImpl.toLocalId(entry.element("id").getText());
	}
	
	public void setEntryValidator(ValidateEntry validator) {
		this.validator = validator;
	}

	private ValidateEntry getEntryValidator() {
		if (null == this.validator) {
			this.validator = new ValidateEntry();
		}
		return this.validator;
	}

	private void sendInvalidEntryResponse()
			throws InterruptedException {
		logger.info("Entry is not valid: '" + entryContent.getErrorMessage() + "'.");
		createExtendedErrorReply(PacketError.Type.modify,
				PacketError.Condition.bad_request, entryContent.getErrorMessage());
		outQueue.put(response);
	}

	private boolean isRequestValid() throws Exception {
		item = request.getChildElement().element("publish").element("item");
		if (null == item) {
			createExtendedErrorReply(PacketError.Type.modify,
					PacketError.Condition.bad_request, MISSING_ITEM_ELEMENT);
			outQueue.put(response);
			return false;
		}
		entryContent = getEntryValidator();
		entryContent.setEntry(item.element("entry"));
		entryContent.setUser(publishersJID);
		entryContent.setTo(request.getTo().toBareJID());
		entryContent.setNode(node);

		if (!entryContent.isValid()) {
			sendInvalidEntryResponse();
			return false;
		}
		return true;
	}

	private boolean userCanPost() throws NodeStoreException,
			InterruptedException {

		Subscriptions possibleExistingSubscription = channelManager
				.getUserSubscription(node, publishersJID).getSubscription();

		Affiliations possibleExistingAffiliation = channelManager
				.getUserAffiliation(node, publishersJID).getAffiliation();

		if ((false == possibleExistingSubscription
				.equals(Subscriptions.subscribed))
				|| (false == possibleExistingAffiliation.in(
						Affiliations.moderator, Affiliations.owner,
						Affiliations.publisher))) {
			response.setType(Type.error);
			PacketError error = new PacketError(
					PacketError.Condition.forbidden, PacketError.Type.auth);
			response.setError(error);
			outQueue.put(response);
			return false;
		}
		return true;
	}

	private void sendResponseStanza() throws InterruptedException {
		/*
		 * Success, let's response as defined in
		 * http://xmpp.org/extensions/xep-0060.html#publisher-publish - 7.1.2
		 * Success Case
		 */
		Element pubsub = new DOMElement(PubSubSet.ELEMENT_NAME,
				new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));

		Element publish = pubsub.addElement("publish");
		publish.addAttribute("node", node);

		Element newItem = publish.addElement("item");
		newItem.addAttribute("id", id);

		response.setChildElement(pubsub);
		outQueue.put(response);
	}

	private boolean nodeExists() throws Exception {
		if (true == channelManager.nodeExists(node))
			return true;
		response.setType(Type.error);
		PacketError error = new PacketError(
				PacketError.Condition.item_not_found, PacketError.Type.cancel);
		response.setError(error);
		outQueue.put(response);
		return false;
	}

	private boolean checkNode() throws InterruptedException, NodeStoreException {
		if ((node == null) || (true == node.equals(""))) {
			response.setType(Type.error);

			Element badRequest = new DOMElement(PacketError.Condition.bad_request.toXMPP(),
					new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

			Element nodeIdRequired = new DOMElement(NODE_ID_REQUIRED,
					new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));

			Element error = new DOMElement("error");
			error.addAttribute("type", PacketError.Type.modify.toXMPP());
			error.add(badRequest);
			error.add(nodeIdRequired);

			response.setChildElement(error);

			outQueue.put(response);
			return false;
		}
		boolean isLocalNode = false;
		try {
			isLocalNode = channelManager.isLocalNode(node);
		} catch (IllegalArgumentException e) {
			response.setType(Type.error);
			PacketError pe = new PacketError(
					PacketError.Condition.bad_request,
					PacketError.Type.modify);
			response.setError(pe);
			logger.error(e);
			outQueue.put(response);
			return false;
		}

		if (false == isLocalNode) {
			makeRemoteRequest();
			return false;
		}
		return true;
	}

	private void sendNotifications() throws NodeStoreException,
			InterruptedException {
		// Let's send notifications as defined in 7.1.2.1 Notification With
		// Payload
		Message msg = new Message();
		msg.getElement().addAttribute("remote-server-discover", "false");
		msg.setType(Message.Type.headline);
		msg.setFrom(request.getTo());
		msg.getElement().addAttribute("remote-server-discover", "false");
		Element event = msg.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		Element items = event.addElement("items");
		items.addAttribute("node", node);
		Element i = items.addElement("item");
		i.addAttribute("id", id);
		i.add(entry.createCopy());

		ResultSet<NodeSubscription> cur = channelManager
				.getNodeSubscriptionListeners(node);

		for (NodeSubscription ns : cur) {
			JID to = ns.getUser();
			if (ns.getSubscription().equals(Subscriptions.subscribed)) {
				logger.debug("Sending post notification to " + to.toBareJID());
				msg.setTo(ns.getListener());
				outQueue.put(msg.createCopy());
			}
		}

		Collection<JID> admins = getAdminUsers();
		for (JID admin : admins) {
			msg.setTo(admin);
			outQueue.put(msg.createCopy());
		}
	}

	private void makeRemoteRequest() throws InterruptedException {
		request.setTo(new JID(node.split("/")[2]).getDomain());
		Element actor = request.getElement().element("pubsub")
				.addElement("actor", JabberPubsub.NS_BUDDYCLOUD);
		actor.addText(request.getFrom().toBareJID());
		outQueue.put(request);
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("publish");
	}
}