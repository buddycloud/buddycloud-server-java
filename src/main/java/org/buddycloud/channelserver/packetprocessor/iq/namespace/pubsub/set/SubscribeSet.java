
package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.NodeStore.Transaction;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubSet;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.event.Event;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscribeSet extends PubSubElementProcessorAbstract {
	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;

	private IQ request;
	private String node;
	private JID subscribingJid;

	private static final Logger logger = Logger.getLogger(SubscribeSet.class);

	public SubscribeSet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		node = elm.attributeValue("node");
		request = reqIQ;
		if ((node == null) || (true == node.equals(""))) {
			missingNodeName();
			return;
		}
        if (false == channelManager.isLocalNode(node)) {
        	makeRemoteRequest();
        	return;
        }
		subscribingJid = request.getFrom();
		boolean isLocalNode = true;
		boolean isLocalSubscriber = false;

		if (actorJID != null) {
			subscribingJid = actorJID;
		} else {
			isLocalSubscriber = channelManager.isLocalJID(subscribingJid);
			// Check that user is registered.
			if (!isLocalSubscriber) {
				failAuthRequired();
				return;
			}
		}

		// 6.1.3.1 JIDs Do Not Match

		// Covers where we have juliet@shakespeare.lit/the-balcony
		String[] jidParts = elm.attributeValue("jid").split("/");
		String jid = jidParts[0];
		if (false == subscribingJid.toBareJID().equals(jid)) {

			/*
			 * // 6.1.3.1 JIDs Do Not Match <iq type='error'
			 * from='pubsub.shakespeare.lit' to='francisco@denmark.lit/barracks'
			 * id='sub1'> <error type='modify'> <bad-request
			 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> <invalid-jid
			 * xmlns='http://jabber.org/protocol/pubsub#errors'/> </error> </iq>
			 */

			IQ reply = IQ.createResultIQ(request);
			reply.setType(Type.error);

			Element badRequest = new DOMElement("bad-request",
					new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
			Element nodeIdRequired = new DOMElement("invalid-jid",
					new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
			Element error = new DOMElement("error");
			error.addAttribute("type", PacketError.Type.modify.toXMPP());
			error.add(badRequest);
			error.add(nodeIdRequired);
			reply.setChildElement(error);
			outQueue.put(reply);
			return;
		}

		if (false == channelManager.nodeExists(node)) {
			IQ reply = IQ.createResultIQ(request);
			reply.setType(Type.error);
			PacketError pe = new PacketError(
					PacketError.Condition.item_not_found,
					PacketError.Type.cancel);
			reply.setError(pe);
			outQueue.put(reply);
			return;
		}

		// If node is whitelist
		// 6.1.3.4 Not on Whitelist

		// Subscribe to a node.

		Transaction t = null;
		try {
			t = channelManager.beginTransaction();

			NodeSubscription nodeSubscription = channelManager
					.getUserSubscription(node, subscribingJid);
			NodeAffiliation nodeAffiliation = channelManager
					.getUserAffiliation(node, subscribingJid);

			Affiliations possibleExistingAffiliation = nodeAffiliation
					.getAffiliation();
			Subscriptions possibleExistingSubscription = nodeSubscription
					.getSubscription();

			if (Affiliations.outcast.toString().equals(
					possibleExistingAffiliation.toString())) {
				/*
				 * 6.1.3.8 Blocked <iq type='error'
				 * from='pubsub.shakespeare.lit'
				 * to='francisco@denmark.lit/barracks' id='sub1'> <error
				 * type='auth'> <forbidden
				 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> </error> </iq>
				 */
				IQ reply = IQ.createResultIQ(request);
				reply.setType(Type.error);
				PacketError pe = new PacketError(
						org.xmpp.packet.PacketError.Condition.forbidden,
						org.xmpp.packet.PacketError.Type.auth);
				reply.setError(pe);
				outQueue.put(reply);
				return;
			}

			if (false == possibleExistingSubscription.toString().equals(
					Subscriptions.none.toString())) {
				logger.debug("User already has a '"
						+ possibleExistingSubscription.toString()
						+ "' subscription");
				tooManySubscriptions();
				return;
			}

			// Finally subscribe to the node :-)
			Map<String, String> nodeConf = channelManager.getNodeConf(node);
			Affiliations defaultAffiliation = null;
			try {
				defaultAffiliation = Affiliations.createFromString(nodeConf
						.get(Conf.DEFAULT_AFFILIATION));
			} catch (NullPointerException e) {
				e.printStackTrace();
				defaultAffiliation = Affiliations.member;
			}
			Subscriptions defaultSubscription = Subscriptions.subscribed;
			if (true == nodeConf.get(Conf.ACCESS_MODEL).equals(
					AccessModels.authorize.toString())) {
				defaultSubscription = Subscriptions.pending;
			}

			NodeSubscription newSubscription = new NodeSubscriptionImpl(node,
					subscribingJid, request.getFrom(), defaultSubscription);
			channelManager.addUserSubscription(newSubscription);

			channelManager.setUserAffiliation(node, subscribingJid,
					defaultAffiliation);

			IQ reply = IQ.createResultIQ(request);
			Element pubsub = reply.setChildElement(PubSubSet.ELEMENT_NAME,
					JabberPubsub.NAMESPACE_URI);
			pubsub.addElement("subscription")
					.addAttribute("node", node)
					.addAttribute("jid", subscribingJid.toBareJID())
					.addAttribute("subscription",
							defaultSubscription.toString());
			pubsub.addElement("affiliation").addAttribute("node", node)
					.addAttribute("jid", subscribingJid.toBareJID())
					.addAttribute("affiliation", defaultAffiliation.toString());

			outQueue.put(reply);

			notifySubscribers(defaultSubscription, defaultAffiliation);

			t.commit();
		} finally {
			if (t != null)
				t.close();
		}
	}

	private void makeRemoteRequest() throws InterruptedException {
		request.setTo(new JID(node.split("/")[2]).getDomain());
		Element actor = request.getElement()
		    .element("pubsub")
		    .addElement("actor", JabberPubsub.NS_BUDDYCLOUD);
		actor.addText(request.getFrom().toBareJID());
	    outQueue.put(request);
	}

	private void notifySubscribers(Subscriptions subscriptionStatus,
			Affiliations affiliationType) throws NodeStoreException,
			InterruptedException {

		ResultSet<NodeSubscription> subscribers = channelManager
				.getNodeSubscriptionListeners(node);

		// Get all the affiliated users (so we can work out moderators)
		ResultSet<NodeAffiliation> nodeAffiliations = channelManager
				.getNodeAffiliations(node);
		HashSet<JID> moderatorOwners = new HashSet<JID>();

		for (NodeAffiliation nodeAffiliation : nodeAffiliations) {
			if (nodeAffiliation.getAffiliation().in(Affiliations.owner,
					Affiliations.moderator)) {
				moderatorOwners.add(nodeAffiliation.getUser());
			}
		}

		Document document = getDocumentHelper();
		Element message = document.addElement("message");
		Element event = message.addElement("event");
		Element subscription = event.addElement("subscription");
		event.addNamespace("", Event.NAMESPACE);
		message.addAttribute("id", request.getID());
		message.addAttribute("from", request.getTo().toString());
		message.addAttribute("type", "headline");
		message.addNamespace("", "jabber:client");
		subscription
				.addAttribute("subscription", subscriptionStatus.toString());
		subscription.addAttribute("jid", subscribingJid.toBareJID());
		subscription.addAttribute("node", node);

		if (true == subscriptionStatus.in(Subscriptions.subscribed, Subscriptions.pending)) {
			Element affiliation = event.addElement("affiliation");
			affiliation.addAttribute("node", node);
			affiliation.addAttribute("jid", subscribingJid.toBareJID());
			affiliation.addAttribute("affiliation", affiliationType.toString());
		}
		Message rootElement = new Message(message);

		for (NodeSubscription subscriber : subscribers) {
			if (moderatorOwners.contains(subscriber.getUser())
					&& subscriptionStatus.equals(Subscriptions.pending)) {
				outQueue.put(getPendingSubscriptionNotification(subscriber
						.getUser().toBareJID(), subscribingJid.toBareJID()));
			} else {
				Message notification = rootElement.createCopy();
				notification.setTo(subscriber.getListener());
				notification.setID(notification.getID() + "-1");
				outQueue.put(notification);
			}
		}
	}

	private Message getPendingSubscriptionNotification(String receiver, String subscriber) {

		Document document = getDocumentHelper();
		Element message = document.addElement("message");
		message.addAttribute("id", request.getID() + "-1");
		message.addAttribute("from", request.getTo().toString());
		message.addAttribute("type", "headline");
		message.addAttribute("to",  receiver);
		message.addNamespace("", "jabber:client");
		DataForm dataForm = new DataForm(DataForm.Type.form);
		dataForm.addInstruction("Allow " + subscriber
				+ " to subscribe to node " + node + "?");
		dataForm.setTitle("Confirm channel subscription");
		FormField formType = dataForm.addField();
		formType.addValue(JabberPubsub.NS_AUTHORIZATION);
		formType.setType(FormField.Type.hidden);
		formType.setVariable("FORM_TYPE");
		FormField subscribingNode = dataForm.addField();
		subscribingNode.setType(FormField.Type.text_single);
		subscribingNode.setVariable(JabberPubsub.VAR_NODE);
		subscribingNode.setLabel("Node");
		subscribingNode.addValue(node);
		FormField jid = dataForm.addField();
		jid.setType(FormField.Type.jid_single);
		jid.addValue(subscriber);
		jid.setLabel("Subscriber Address");
		jid.setVariable(JabberPubsub.VAR_SUBSCRIBER_JID);
		FormField allow = dataForm.addField();
		allow.setLabel("Allow?");
		allow.setVariable(JabberPubsub.VAR_ALLOW);
		allow.addValue("false");
		allow.setType(FormField.Type.boolean_type);
		message.add(dataForm.getElement());
		return new Message(message);
	}

	private void tooManySubscriptions() throws InterruptedException {
		/*
		 * 6.1.3.9 Too Many Subscriptions <iq type='error'
		 * from='pubsub.shakespeare.lit' to='francisco@denmark.lit/barracks'
		 * id='sub1'> <error type='wait'> <policy-violation
		 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> <too-many-subscriptions
		 * xmlns='http://jabber.org/protocol/pubsub#errors'/> </error> </iq>
		 */

		IQ reply = IQ.createResultIQ(request);
		reply.setType(Type.error);

		Element badRequest = new DOMElement("policy-violation",
				new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
		Element nodeIdRequired = new DOMElement("too-many-subscriptions",
				new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
		Element error = new DOMElement("error");
		error.addAttribute("type", "wait");
		error.add(badRequest);
		error.add(nodeIdRequired);
		reply.setChildElement(error);
		outQueue.put(reply);
	}

	private void failAuthRequired() throws InterruptedException {
		// If the packet did not have actor, and the sender is not a local user
		// subscription is not allowed.

		/*
		 * <iq type='error' from='pubsub.shakespeare.lit'
		 * to='hamlet@denmark.lit/elsinore' id='create1'> <error type='auth'>
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

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("subscribe");
	}
}