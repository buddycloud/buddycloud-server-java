package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.Iterator;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.event.Event;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class AffiliationEvent extends PubSubElementProcessorAbstract {

	Element requestedAffiliation;
	NodeSubscriptionImpl currentAffiliation;

	private static final Logger LOGGER = Logger
			.getLogger(AffiliationEvent.class);

	/**
	 * Constructor
	 * 
	 * @param outQueue
	 *            Outgoing message queue
	 * @param dataStore
	 *            Data Access Object (DAO)
	 */
	public AffiliationEvent(BlockingQueue<Packet> outQueue, DataStore dataStore) {
		setDataStore(dataStore);
		setOutQueue(outQueue);
	}

	/**
	 * Process incoming stanza
	 */
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		element = elm;
		response = IQ.createResultIQ(reqIQ);
		request = reqIQ;
		actor = actorJID;
		node = element.attributeValue("node");

		if (actor == null) {
			actor = request.getFrom();
		}
		try {
			if ((false == nodeProvided()) || (false == validRequestStanza())
					|| (false == checkNodeExists())
					|| (false == actorHasPermissionToAuthorize())
					|| (false == subscriberHasCurrentAffiliation())
					|| (false == attemptToChangeAffiliationOfNodeOwner())) {
				outQueue.put(response);
				return;
			}
			saveUpdatedSubscription();
			sendNotifications();
		} catch (DataStoreException e) {
			LOGGER.debug(e);
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
			outQueue.put(response);
			return;
		}
	}

	private boolean attemptToChangeAffiliationOfNodeOwner() {
		if (false == currentAffiliation.getAffiliation().equals(
				Affiliations.owner.toString())) {
			return true;
		}
		setErrorCondition(PacketError.Type.modify,
				PacketError.Condition.not_acceptable);
		return false;
	}

	private void sendNotifications() throws Exception {
		Iterator<? extends NodeSubscription> subscribers = dataStore
				.getNodeSubscribers(node);
		if (null == subscribers) {
			return;
		}
		Document document = getDocumentHelper();
		Element message = document.addElement("message");
		Element event = message.addElement("event");
		Element subscription = event.addElement("affiliation");
		event.addNamespace("", JabberPubsub.NS_PUBSUB_EVENT);
		message.addAttribute("id", request.getID());
		message.addAttribute("from", request.getTo().toString());
		subscription.addAttribute("node", node);
		subscription.addAttribute("jid",
				requestedAffiliation.attributeValue("jid"));
		subscription.addAttribute("affiliation",
				requestedAffiliation.attributeValue("affiliation"));
		Message rootElement = new Message(message);

		while (true == subscribers.hasNext()) {
			String subscriber = subscribers.next().getBareJID();
			message.addAttribute("to", subscriber);
			Message notification = rootElement.createCopy();
			outQueue.put(notification);
		}

	}

	private void saveUpdatedSubscription() throws DataStoreException {
		dataStore.unsubscribeUserFromNode(
				requestedAffiliation.attributeValue("jid"), node);
		dataStore.subscribeUserToNode(
				requestedAffiliation.attributeValue("jid"), node,
				requestedAffiliation.attributeValue("affiliation"),
				currentAffiliation.getSubscription(),
				currentAffiliation.getForeignChannelServer());
	}

	private boolean nodeProvided() {
		if (null != node) {
			return true;
		}
		response.setType(IQ.Type.error);
		Element nodeIdRequired = new DOMElement("nodeid-required",
				new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
		Element badRequest = new DOMElement(
				PacketError.Condition.bad_request.toString(), new Namespace("",
						JabberPubsub.NS_XMPP_STANZAS));
		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		error.add(badRequest);
		error.add(nodeIdRequired);
		response.setChildElement(error);
		return false;
	}

	private boolean validRequestStanza() {
		try {
			requestedAffiliation = request.getElement().element("pubsub")
					.element("affiliations").element("affiliation");
			if ((null == requestedAffiliation)
					|| (null == requestedAffiliation.attribute("jid"))
					|| (null == requestedAffiliation.attribute("affiliation"))) {
				setErrorCondition(PacketError.Type.modify,
						PacketError.Condition.bad_request);
				return false;
			}
		} catch (NullPointerException e) {
			LOGGER.debug(e);
			setErrorCondition(PacketError.Type.modify,
					PacketError.Condition.bad_request);
			return false;
		}
		requestedAffiliation.addAttribute(
				"affiliation",
				Affiliations.createFromString(
						requestedAffiliation.attributeValue("affiliation"))
						.toString());
		return true;
	}

	private boolean subscriberHasCurrentAffiliation() throws DataStoreException {
		currentAffiliation = dataStore.getUserSubscriptionOfNode(
				requestedAffiliation.attributeValue("jid"), node);
		if (null == currentAffiliation) {
			setErrorCondition(PacketError.Type.modify,
					PacketError.Condition.unexpected_request);
			return false;
		}
		return true;
	}

	private boolean actorHasPermissionToAuthorize() throws DataStoreException {
		NodeSubscriptionImpl subscription = dataStore
				.getUserSubscriptionOfNode(actor.toBareJID(), node);
		if (null == subscription) {
			setErrorCondition(PacketError.Type.auth,
					PacketError.Condition.not_authorized);
			return false;
		}
		if ((false == subscription.getAffiliation().equals(
				Affiliations.moderator.toString()))
				&& (false == subscription.getAffiliation().equals(
						Affiliations.owner.toString()))) {
			setErrorCondition(PacketError.Type.auth,
					PacketError.Condition.not_authorized);
			return false;
		}
		return true;
	}

	private boolean checkNodeExists() throws DataStoreException {
		if (false == dataStore.nodeExists(node)) {
			setErrorCondition(PacketError.Type.cancel,
					PacketError.Condition.item_not_found);
			return false;
		}
		return true;
	}

	/**
	 * Determine if this class is capable of processing incoming stanza
	 */
	public boolean accept(Element elm) {
		return elm.getName().equals("affiliations");
	}
}