package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.BlockingQueue;

import org.apache.commons.lang.time.DateFormatUtils;
import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ValidateEntry;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubSet;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
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

public class PublishSet extends PubSubElementProcessorAbstract {

	private static final Logger LOGGER = Logger.getLogger(PublishSet.class);

	private static final SimpleDateFormat ISO_DATE_FORMAT = new SimpleDateFormat(
			DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern());

	private static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SZZ";

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;
	private IQ requestIq;
	private String node;
	private Element entry;
	private String id;
	private IQ response;
	private Date updated;
	private JID publishersJID;
	private String inReplyTo;

	public PublishSet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws InterruptedException, NodeStoreException {
		
		node = elm.attributeValue("node");
		requestIq = reqIQ;
		response = IQ.createResultIQ(reqIQ);
		publishersJID = requestIq.getFrom();
		if (false == checkNode()) return;

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

		if (false == nodeExists()) return;
		if (false == userCanPost()) return;
		Element item = elm.element("item");
		if (false == isRequestValid(item)) return;
		if (false == extractItemDetails(item)) return;
		if (false == determineInReplyToDetails()) return;
		
		saveNodeItem();
		sendResponseStanza();
		sendNotifications();

	}

	private void saveNodeItem() throws NodeStoreException {
		// Let's store the new item.
		channelManager.addNodeItem(new NodeItemImpl(node, id, updated,
				entry.asXML(), inReplyTo));
	}

	private boolean determineInReplyToDetails() throws NodeStoreException, InterruptedException {
		inReplyTo = null;
		Element reply;
		NodeItem nodeItem = null;
		
		if (null == (reply = entry.element("in-reply-to"))) return true;

		String[] inReplyToParts = reply.attributeValue("ref").split(",");
		inReplyTo = inReplyToParts[inReplyToParts.length - 1];
		
		if (null == (nodeItem = channelManager.getNodeItem(node, inReplyTo))) {
			response.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.item_not_found,
					org.xmpp.packet.PacketError.Type.cancel);
			response.setError(pe);
			outQueue.put(response);
			return false;
		}
		if (null != nodeItem.getInReplyTo()) {
			LOGGER.error("User is attempting to reply to a reply");
			response.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.bad_request,
					org.xmpp.packet.PacketError.Type.modify);
			response.setError(pe);
			outQueue.put(response);
			return false;
        }
		return true;
	}

	private boolean extractItemDetails(Element item) throws InterruptedException {
		ValidateEntry vEntry = new ValidateEntry(item.element("entry"));
		if (!vEntry.isValid()) {
			sendInvalidEntryResponse(vEntry);
			return false;
		}

		entry = vEntry.createBcCompatible(publishersJID.toBareJID(),
				requestIq.getTo().toBareJID(), node);

		id = entry.element("id").getText();
		String[] idParts = id.split(",");
		id = idParts[2];

		String updatedText = entry.elementText("updated");

		if (updatedText == null || updatedText.isEmpty()) {
			updatedText = entry.elementText("published");
		}

		if (updatedText == null || updatedText.isEmpty()) {
			updated = new Date(); // Default to now
		} else {
			try {
				updated = new SimpleDateFormat(DATE_FORMAT).parse(updatedText);
			} catch (ParseException e) {
				updated = new Date();

				LOGGER.error("Invalid date encountered in atom entry: "
						+ updatedText);
				// Otherwise we will just let it pass
			}
		}
		return true;
	}

	private void sendInvalidEntryResponse(ValidateEntry vEntry)
			throws InterruptedException {
		LOGGER.info("Entry is not valid: '" + vEntry.getErrorMsg() + "'.");

		/*
		 * <iq type='error' from='pubsub.shakespeare.lit'
		 * to='hamlet@denmark.lit/elsinore' id='publish1'> <error
		 * type='modify'> <bad-request
		 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> <invalid-payload
		 * xmlns='http://jabber.org/protocol/pubsub#errors'/> </error> </iq>
		 */
		response.setType(Type.error);

		Element badRequest = new DOMElement("bad-request",
				new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

		Element nodeIdRequired = new DOMElement("invalid-payload",
				new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));

		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		error.add(badRequest);
		error.add(nodeIdRequired);

		response.setChildElement(error);

		outQueue.put(response);
	}

	private boolean isRequestValid(Element item) throws InterruptedException {
		if (item != null) return true;
		
		response.setType(Type.error);

		Element badRequest = new DOMElement("bad-request",
				new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

		Element nodeIdRequired = new DOMElement("item-required",
				new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));

		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		error.add(badRequest);
		error.add(nodeIdRequired);

		response.setChildElement(error);

		outQueue.put(response);
		return false;
	}

	private boolean userCanPost() throws NodeStoreException, InterruptedException {
		NodeSubscription nodeSubscription = channelManager.getUserSubscription(
				node, publishersJID);
		Subscriptions possibleExistingSubscription = nodeSubscription
				.getSubscription();

		NodeAffiliation nodeaffiliation = channelManager.getUserAffiliation(
				node, publishersJID);
		Affiliations possibleExistingAffiliation = nodeaffiliation
				.getAffiliation();

		if ((false == possibleExistingSubscription.equals(
				Subscriptions.subscribed))
				|| (false == possibleExistingAffiliation.in(Affiliations.moderator,
						Affiliations.owner, Affiliations.publisher))) {
			response.setType(Type.error);
			PacketError error = new PacketError(
					PacketError.Condition.forbidden,
					PacketError.Type.auth);
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

	private boolean nodeExists() throws NodeStoreException, InterruptedException {
		if (true == channelManager.nodeExists(node)) return true;
		response.setType(Type.error);
		PacketError error = new PacketError(
				PacketError.Condition.item_not_found,
				PacketError.Type.cancel);
		response.setError(error);
		outQueue.put(response);
		return false;
	}

	private boolean checkNode() throws InterruptedException, NodeStoreException {
		if ((node == null) || (true == node.equals(""))) {
			response.setType(Type.error);
	
			Element badRequest = new DOMElement("bad-request",
					new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
	
			Element nodeIdRequired = new DOMElement("nodeid-required",
					new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
	
			Element error = new DOMElement("error");
			error.addAttribute("type", "modify");
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
					org.xmpp.packet.PacketError.Condition.bad_request,
					org.xmpp.packet.PacketError.Type.modify);
			response.setError(pe);
			outQueue.put(response);
			return false;
		}
		
		if (false == isLocalNode) {
			makeRemoteRequest();
			return false;
		}
		return true;
	}

	private void sendNotifications()
			throws NodeStoreException, InterruptedException {
		// Let's send notifications as defined in 7.1.2.1 Notification With
		// Payload
		Message msg = new Message();
		msg.getElement().addAttribute("remote-server-discover", "false");
		msg.setType(Message.Type.headline);
		msg.setFrom(requestIq.getTo());
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
			    LOGGER.debug("Sending post notification to " + to.toBareJID());
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
		requestIq.setTo(new JID(node.split("/")[2]).getDomain());
		Element actor = requestIq.getElement()
		    .element("pubsub")
		    .addElement("actor", JabberPubsub.NS_BUDDYCLOUD);
		actor.addText(requestIq.getFrom().toBareJID());
	    outQueue.put(requestIq);
	}
	
	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("publish");
	}
}