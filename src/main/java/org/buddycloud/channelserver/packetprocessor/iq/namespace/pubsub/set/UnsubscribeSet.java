package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.Collection;
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
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
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
	public static final String NODE_ID_REQUIRED = "nodeid-required";

	private String node;
	private IQ request;
	private IQ response;
	private JID unsubscribingJid;

	public UnsubscribeSet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {

		request = reqIQ;
		node = request.getChildElement().element("unsubscribe").attributeValue("node");
		response = IQ.createResultIQ(request);

		if ((node == null) || (node.equals(""))) {
			missingNodeName();
			return;
		}

		JID from = request.getFrom();
		if ((false == node.equals("/firehose")) && (false == channelManager.isLocalNode(node))) {
			makeRemoteRequest();
			return;
		}
		boolean isLocalSubscriber = false;

		if (actorJID != null) {
			from = actorJID;
		}
		
		unsubscribingJid = new JID(request.getChildElement().element("unsubscribe")
				.attributeValue("jid"));
		
		if (false == unsubscribingJid.toBareJID().equals(from.toBareJID())) {
		    failAuthRequired();
		    return;
		}


		if (false == channelManager.nodeExists(node)) {
			response.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.item_not_found,
					org.xmpp.packet.PacketError.Type.cancel);
			response.setError(pe);
			outQueue.put(response);
			return;
		}

		NodeSubscription existingSubscription = channelManager
				.getUserSubscription(node, unsubscribingJid);
		NodeAffiliation existingAffiliation = channelManager
				.getUserAffiliation(node, unsubscribingJid);
		String fromJID = request.getFrom().toBareJID();

		// Check that the requesting user is allowed to unsubscribe according to
		// XEP-0060 section 6.2.3.3		
		if (false == unsubscribingJid.equals(existingSubscription.getUser())) {
			response.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.forbidden,
					org.xmpp.packet.PacketError.Type.auth);
			response.setError(pe);
			outQueue.put(response);
			return;
		}
		
		if ((Affiliations.owner == existingAffiliation.getAffiliation()) &&
			(channelManager.getNodeOwners(node).size() < 2)) {
			
			response.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.not_allowed,
					org.xmpp.packet.PacketError.Type.cancel);
			response.setError(pe);
			outQueue.put(response);
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

		outQueue.put(response);
		notifySubscribers();
	}

	private void notifySubscribers() throws NodeStoreException,
			InterruptedException {
		ResultSet<NodeSubscription> subscribers = channelManager
				.getNodeSubscriptionListeners(node);

		Document document = getDocumentHelper();
		Element message = document.addElement("message");
		message.addAttribute("remote-server-discover", "false");
		Element event = message.addElement("event", Event.NAMESPACE);
		Element subscription = event.addElement("subscription");
		subscription.addAttribute("node", node);
		subscription.addAttribute("jid", unsubscribingJid.toBareJID());
		subscription
				.addAttribute("subscription", Subscriptions.none.toString());
		message.addAttribute("from", request.getTo().toString());
		message.addAttribute("type", "headline");
		// "None" because we don't glorify the bad
		Element affiliations = event.addElement("affiliations");
		Element affiliation = affiliations.addElement("affiliation");
		affiliation.addAttribute("jid", unsubscribingJid.toBareJID());
		affiliation.addAttribute("node", node);

		Message rootElement = new Message(message);
        
		for (NodeSubscription subscriber : subscribers) {
			Message notification = rootElement.createCopy();
			notification.setTo(subscriber.getUser());
			outQueue.put(notification);
		}
		
		Collection<JID> admins = getAdminUsers();
		for (JID admin : admins) {
			Message notification = rootElement.createCopy();
			notification.setTo(admin);
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
		response.setType(Type.error);
		PacketError pe = new PacketError(
				org.xmpp.packet.PacketError.Condition.not_authorized,
				org.xmpp.packet.PacketError.Type.auth);
		response.setError(pe);
		outQueue.put(response);
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

		response.setType(Type.error);

		Element badRequest = new DOMElement("bad-request",
				new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));

		Element nodeIdRequired = new DOMElement(NODE_ID_REQUIRED,
				new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));

		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		error.add(badRequest);
		error.add(nodeIdRequired);

		response.setChildElement(error);

		outQueue.put(response);
	}

	private void makeRemoteRequest() throws InterruptedException {
		request.setTo(new JID(node.split("/")[2]).getDomain());
		Element actor = request.getElement()
		    .element("pubsub")
		    .addElement("actor", Buddycloud.NS);
		actor.addText(request.getFrom().toBareJID());
	    outQueue.put(request);
	}
	
	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("unsubscribe");
	}
}