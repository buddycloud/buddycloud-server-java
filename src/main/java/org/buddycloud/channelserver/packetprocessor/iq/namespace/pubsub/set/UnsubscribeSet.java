package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.event.Event;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class UnsubscribeSet extends PubSubElementProcessorAbstract {

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;

	private static final Logger logger = Logger.getLogger(UnsubscribeSet.class);

	private String node;
	private IQ request;
	private JID unsubscribingJid;

	public UnsubscribeSet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {

		node = elm.attributeValue("node");
		request = reqIQ;

		if ((node == null) || (node.equals(""))) {
			missingNodeName();
			return;
		}

		unsubscribingJid = request.getFrom();
		if (false == channelManager.isLocalNode(node)) {
			makeRemoteRequest();
			return;
		}
		boolean isLocalSubscriber = false;

		if (actorJID != null) {
			unsubscribingJid = actorJID;
		} else {
			isLocalSubscriber = channelManager.isLocalJID(unsubscribingJid);
			// Check that user is registered.
			if (!isLocalSubscriber) {
				failAuthRequired();
				return;

			}
		}

		if (false == channelManager.nodeExists(node)) {
			IQ reply = IQ.createResultIQ(request);
			reply.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.item_not_found,
					org.xmpp.packet.PacketError.Type.cancel);
			reply.setError(pe);
			outQueue.put(reply);
			return;
		}

		NodeSubscription existingSubscription = channelManager
				.getUserSubscription(node, unsubscribingJid);
		NodeAffiliation existingAffiliation = channelManager
				.getUserAffiliation(node, unsubscribingJid);
		String fromJID = request.getFrom().toBareJID();

		// Check that the requesting user is allowed to unsubscribe according to
		// XEP-0060 section 6.2.3.3
		if (false == fromJID.equals(existingSubscription.getUser().toBareJID())) {
			IQ reply = IQ.createResultIQ(request);
			reply.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.forbidden,
					org.xmpp.packet.PacketError.Type.auth);
			reply.setError(pe);
			outQueue.put(reply);
			return;
		}

		NodeSubscription newSubscription = new NodeSubscriptionImpl(
				existingSubscription.getNodeId(),
				existingSubscription.getUser(),
				existingSubscription.getListener(), Subscriptions.none);

		channelManager.addUserSubscription(newSubscription);
		if (false == Affiliations.outcast.toString().equals(
				existingAffiliation.getAffiliation().toString())) {
			channelManager.setUserAffiliation(node, unsubscribingJid,
					Affiliations.none);
		}

		IQ reply = IQ.createResultIQ(request);
		outQueue.put(reply);
		notifySubscribers();
	}

	private void notifySubscribers() throws NodeStoreException,
			InterruptedException {
		ResultSet<NodeSubscription> subscribers = channelManager
				.getNodeSubscriptionListeners(node);

		Document document = getDocumentHelper();
		Element message = document.addElement("message");
		Element event = message.addElement("event");
		Element subscription = event.addElement("subscription");
		Element affiliation = event.addElement("affiliation");
		subscription.addAttribute("node", node);
		subscription.addAttribute("jid", unsubscribingJid.toBareJID());
		subscription
				.addAttribute("subscription", Subscriptions.none.toString());
		event.addNamespace("", Event.NAMESPACE);
		message.addAttribute("id", request.getID());
		message.addAttribute("from", unsubscribingJid.toBareJID());
		message.addAttribute("type", "headline");
		// "None" because we don't glorify the bad
		affiliation.addAttribute("affiliation", Affiliations.none.toString());
		affiliation.addAttribute("jid", unsubscribingJid.toBareJID());
		affiliation.addAttribute("node", node);

		Message rootElement = new Message(message);

		for (NodeSubscription subscriber : subscribers) {
			Message notification = rootElement.createCopy();
			notification.setTo(subscriber.getUser());
			outQueue.put(notification);
		}
	}

	private void failAuthRequired() throws InterruptedException {
		// If the packet did not have actor, and the sender is not a
		// local user
		// subscription is not allowed.

		/*
		 * <iq type='error' from='pubsub.shakespeare.lit'
		 * to='hamlet@denmark.lit/elsinore' id='unsub1'> <error type='auth'>
		 * <registration-required xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
		 * </error> </iq>
		 */

		IQ reply = IQ.createResultIQ(request);
		reply.setType(Type.error);
		PacketError pe = new PacketError(
				org.xmpp.packet.PacketError.Condition.registration_required,
				org.xmpp.packet.PacketError.Type.auth);
		reply.setError(pe);
		outQueue.put(reply);
	}

	private void missingNodeName() throws InterruptedException {
		/*
		 * 7.2.3.3 NodeID Required
		 * 
		 * <iq type='error' from='pubsub.shakespeare.lit'
		 * to='hamlet@denmark.lit/elsinore' id='retract1'> <error type='modify'>
		 * <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
		 * <nodeid-required xmlns='http://jabber.org/protocol/pubsub#errors'/>
		 * </error> </iq>
		 */

		IQ reply = IQ.createResultIQ(request);
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
	}

	private void makeRemoteRequest() throws InterruptedException {
		request.setTo(new JID(node.split("/")[2]).getDomain());
		Element actor = request.getElement()
		    .element("pubsub")
		    .addElement("actor", JabberPubsub.NS_BUDDYCLOUD);
		actor.addText(request.getFrom().toBareJID());
	    outQueue.put(request);
	}
	
	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("unsubscribe");
	}
}