package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.text.ParseException;
import java.text.SimpleDateFormat;
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
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubSet;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
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

public class PublishSet implements PubSubElementProcessor {

	private static final Logger LOGGER = Logger.getLogger(PublishSet.class);

	private static final SimpleDateFormat ISO_DATE_FORMAT = new SimpleDateFormat(
			DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern());

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;
	private IQ requestIq;
	private String node;

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
		
		if (node == null || node.equals("")) {

			/*
			 * 7.2.3.3 NodeID Required
			 * 
			 * <iq type='error' from='pubsub.shakespeare.lit'
			 * to='hamlet@denmark.lit/elsinore' id='retract1'> <error
			 * type='modify'> <bad-request
			 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> <nodeid-required
			 * xmlns='http://jabber.org/protocol/pubsub#errors'/> </error> </iq>
			 */

			IQ reply = IQ.createResultIQ(reqIQ);
			reply.setType(Type.error);

			Element badRequest = new DOMElement("bad-request",
					new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

			Element nodeIdRequired = new DOMElement("nodeid-required",
					new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));

			Element error = new DOMElement("error");
			error.addAttribute("type", "modify");
			error.add(badRequest);
			error.add(nodeIdRequired);

			reply.setChildElement(error);

			outQueue.put(reply);
			return;
		}

		JID publishersJID = reqIQ.getFrom();
		boolean isLocalNode = false;
		try {
		    isLocalNode = channelManager.isLocalNode(node);
		} catch (IllegalArgumentException e) {
			IQ reply = IQ.createResultIQ(reqIQ);
			reply.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.bad_request,
					org.xmpp.packet.PacketError.Type.modify);
			reply.setError(pe);
			outQueue.put(reply);
			return;
		}
		
		if (false == channelManager.isLocalNode(node)) {
			makeRemoteRequest();
			return;
		}
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

				IQ reply = IQ.createResultIQ(reqIQ);
				reply.setType(Type.error);
				PacketError pe = new PacketError(
						org.xmpp.packet.PacketError.Condition.registration_required,
						org.xmpp.packet.PacketError.Type.auth);
				reply.setError(pe);
				outQueue.put(reply);
				return;
			}
		}

		if (false == channelManager.nodeExists(node)) {
			IQ reply = IQ.createResultIQ(reqIQ);
			reply.setType(Type.error);
			PacketError error = new PacketError(
					PacketError.Condition.item_not_found,
					PacketError.Type.cancel);
			reply.setError(error);
			outQueue.put(reply);
			return;
		}

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
			IQ reply = IQ.createResultIQ(reqIQ);
			reply.setType(Type.error);
			PacketError error = new PacketError(
					PacketError.Condition.forbidden,
					PacketError.Type.auth);
			reply.setError(error);
			outQueue.put(reply);
			return;
		}

		Element item = elm.element("item");
		if (item == null) {

			/*
			 * No item, let's reply something like this: <iq type='error'
			 * from='pubsub.shakespeare.lit' to='hamlet@denmark.lit/elsinore'
			 * id='publish1'> <error type='modify'> <bad-request
			 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> <item-required
			 * xmlns='http://jabber.org/protocol/pubsub#errors'/> </error> </iq>
			 */

			IQ reply = IQ.createResultIQ(reqIQ);
			reply.setType(Type.error);

			Element badRequest = new DOMElement("bad-request",
					new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

			Element nodeIdRequired = new DOMElement("item-required",
					new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));

			Element error = new DOMElement("error");
			error.addAttribute("type", "modify");
			error.add(badRequest);
			error.add(nodeIdRequired);

			reply.setChildElement(error);

			outQueue.put(reply);
			return;
		}

		ValidateEntry vEntry = new ValidateEntry(item.element("entry"));
		if (!vEntry.isValid()) {
			LOGGER.info("Entry is not valid: '" + vEntry.getErrorMsg() + "'.");

			/*
			 * <iq type='error' from='pubsub.shakespeare.lit'
			 * to='hamlet@denmark.lit/elsinore' id='publish1'> <error
			 * type='modify'> <bad-request
			 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> <invalid-payload
			 * xmlns='http://jabber.org/protocol/pubsub#errors'/> </error> </iq>
			 */

			IQ reply = IQ.createResultIQ(reqIQ);
			reply.setType(Type.error);

			Element badRequest = new DOMElement("bad-request",
					new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

			Element nodeIdRequired = new DOMElement("invalid-payload",
					new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));

			Element error = new DOMElement("error");
			error.addAttribute("type", "modify");
			error.add(badRequest);
			error.add(nodeIdRequired);

			reply.setChildElement(error);

			outQueue.put(reply);
			return;
		}

		Element entry = vEntry.createBcCompatible(publishersJID.toBareJID(),
				reqIQ.getTo().toBareJID(), node);

		String id = entry.element("id").getText();
		String[] idParts = id.split(",");
		id = idParts[2];

		Date updated;

		String updatedText = entry.elementText("updated");

		if (updatedText == null || updatedText.isEmpty()) {
			updatedText = entry.elementText("published");
		}

		if (updatedText == null || updatedText.isEmpty()) {
			updated = new Date(); // Default to now
		} else {
			try {
				updated = new SimpleDateFormat(
						DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT
								.getPattern()).parse(updatedText);
			} catch (ParseException e) {
				updated = new Date();

				LOGGER.error("Invalid date encountered in atom entry: "
						+ updatedText);
				// Otherwise we will just let it pass
			}
		}

		NodeItemImpl nodeItem = new NodeItemImpl(node, id, updated,
				entry.asXML());

		// Let's store the new item.
		channelManager.addNodeItem(nodeItem);

		/*
		 * Success, let's reply as defined in
		 * http://xmpp.org/extensions/xep-0060.html#publisher-publish - 7.1.2
		 * Success Case
		 */
		Element pubsub = new DOMElement(PubSubSet.ELEMENT_NAME,
				new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));

		Element publish = pubsub.addElement("publish");
		publish.addAttribute("node", node);

		Element newItem = publish.addElement("item");
		newItem.addAttribute("id", id);

		IQ result = IQ.createResultIQ(reqIQ);
		result.setChildElement(pubsub);

		outQueue.put(result);

		// Let's send notifications as defined in 7.1.2.1 Notification With
		// Payload
		Message msg = new Message();
		msg.setType(Message.Type.headline);
		msg.setID(reqIQ.getID() + "-1");
		msg.setFrom(requestIq.getTo());
		Element event = msg.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		Element items = event.addElement("items");
		items.addAttribute("node", node);
		Element i = items.addElement("item");
		i.addAttribute("id", id);
		i.add(entry.createCopy());
        Element actor = event.addElement("actor");
        actor.addNamespace("", JabberPubsub.NS_BUDDYCLOUD);

		ResultSet<NodeSubscription> cur = channelManager
				.getNodeSubscriptionListeners(node);
		for (NodeSubscription ns : cur) {
			JID to = ns.getUser();

			// TODO Federation!
			/*
			 * if(ns.getForeignChannelServer() != null) { if(
			 * externalChannelServerReceivers
			 * .contains(ns.getForeignChannelServer()) ) { continue; }
			 * externalChannelServerReceivers.add(ns.getForeignChannelServer());
			 * toBareJID = ns.getForeignChannelServer(); }
			 */
			if (ns.getSubscription().equals(Subscriptions.subscribed)) {
			    LOGGER.debug("Sending post notification to " + to.toBareJID());
			    msg.setTo(ns.getListener());
			    actor.setText(to.toBareJID());
			    outQueue.put(msg.createCopy());
			}
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
