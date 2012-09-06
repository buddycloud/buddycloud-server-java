package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubSet;
import org.buddycloud.channelserver.queue.statemachine.Subscribe;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.event.Event;

public class SubscribeSet extends PubSubElementProcessorAbstract {
	private final BlockingQueue<Packet> outQueue;
	private final DataStore dataStore;

	private IQ     request;
	private String node;
	private JID    subscribingJid;

	public SubscribeSet(BlockingQueue<Packet> outQueue, DataStore dataStore) {
		this.outQueue = outQueue;
		this.dataStore = dataStore;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		node    = elm.attributeValue("node");
		request = reqIQ;
		if ((node == null) || (true == node.equals(""))) {
			missingNodeName();
			return;
		}

		subscribingJid            = request.getFrom();
		boolean isLocalNode       = dataStore.isLocalNode(node);
		boolean isLocalSubscriber = false;

		if (actorJID != null) {
			subscribingJid = actorJID;
		} else {
			isLocalSubscriber = dataStore.isLocalUser(subscribingJid
					.toBareJID());
			// Check that user is registered.
			if (!isLocalSubscriber) {
				failAuthRequired();
				return;
			}
		}

		// 6.1.3.1 JIDs Do Not Match

		// Covers where we have juliet@shakespeare.lit/the-balcony
		String[] jidParts = elm.attributeValue("jid").split("/");
		String jid        = jidParts[0];
		if (!subscribingJid.toBareJID().equals(jid)) {

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
			error.addAttribute("type", "wait");
			error.add(badRequest);
			error.add(nodeIdRequired);
			reply.setChildElement(error);
			outQueue.put(reply);
			return;
		}

		if (!isLocalNode) {
			if (isLocalSubscriber) {
				// Start process to subscribe to external node.
				Subscribe sub = Subscribe.buildSubscribeStatemachine(node,
						request, dataStore);
				outQueue.put(sub.nextStep());
				return;
			}

			// Foreign client is trying to subscribe on a node that does not
			// exists.

			/*
			 * 6.1.3.12 Node Does Not Exist
			 * 
			 * <iq type='error' from='pubsub.shakespeare.lit'
			 * to='francisco@denmark.lit/barracks' id='sub1'> <error
			 * type='cancel'> <item-not-found
			 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> </error> </iq>
			 */

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

		NodeSubscriptionImpl nodeSubscription = dataStore
				.getUserSubscriptionOfNode(subscribingJid.toBareJID(), node);

		String possibleExistingAffiliation = nodeSubscription.getAffiliation();
		String possibleExistingSusbcription = nodeSubscription
				.getSubscription();
		if (Affiliations.outcast.toString().equals(possibleExistingAffiliation)) {
			/*
			 * 6.1.3.8 Blocked <iq type='error' from='pubsub.shakespeare.lit'
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

		if (possibleExistingSusbcription != null) {
			tooManySubscriptions();
			return;
		}

		// Finally subscribe to the node :-)
		HashMap<String, String> nodeConf = dataStore.getNodeConf(node);
		String defaultAffiliation = nodeConf.get(Conf.DEFUALT_AFFILIATION);
		// @todo This should be fixed later...
		String defaultSubscription = Subscriptions.subscribed.toString();

		dataStore.subscribeUserToNode(subscribingJid.toBareJID(), node,
				defaultAffiliation, defaultSubscription,
				isLocalSubscriber ? null : request.getFrom().getDomain());

		IQ reply = IQ.createResultIQ(request);
		Element pubsub = reply.setChildElement(PubSubSet.ELEMENT_NAME,
				JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscription").addAttribute("node", node)
				.addAttribute("jid", subscribingJid.toBareJID())
				.addAttribute("subscription", defaultSubscription);
		pubsub.addElement("affiliation").addAttribute("node", node)
				.addAttribute("jid", subscribingJid.toBareJID())
				.addAttribute("affiliation", defaultAffiliation);

		outQueue.put(reply);

		/*
		 * TODO, send affiliation change message here too. 8.9.4 Notifying
		 * Entities <message from='pubsub.shakespeare.lit'
		 * to='polonius@denmark.lit'> <pubsub
		 * xmlns='http://jabber.org/protocol/pubsub'> <affiliations
		 * node='princely_musings'> <affilation jid='polonius@denmark.lit'
		 * affiliation='none'/> </affiliations> </pubsub> </message>
		 * 
		 * <message type="headline" to="comptest.xmpp.lobstermonster.org"
		 * from="channels.buddycloud.org"> <event
		 * xmlns="http://jabber.org/protocol/pubsub#event"> <subscription
		 * jid="tuomas@xmpp.lobstermonster.org" subscription="subscribed"
		 * node="/user/tuomas@buddycloud.org/posts"/> </event> </message>
		 * 
		 * // check that foreign receivers are handled correctly.
		 */

		Message msg = new Message();
		msg.setTo(reply.getTo());
		msg.setFrom(reply.getFrom());
		msg.setType(Message.Type.headline);
		Element subscription = msg.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT).addElement("subscription");
		subscription.addAttribute("node", node)
				.addAttribute("jid", subscribingJid.toBareJID())
				.addAttribute("subscription", defaultSubscription);
		outQueue.put(msg);

		msg = new Message();
		msg.setTo(reply.getTo());
		msg.setFrom(reply.getFrom());
		msg.setType(Message.Type.headline);
		Element affiliation = msg.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT).addElement("affiliation");
		affiliation.addAttribute("node", node)
				.addAttribute("jid", subscribingJid.toBareJID())
				.addAttribute("affiliation", defaultAffiliation);
		outQueue.put(msg);
		notifySubscribers();
	}

	private void notifySubscribers() throws DataStoreException, InterruptedException
	{
	    Iterator<? extends NodeSubscription> subscribers = dataStore.getNodeSubscribers(node);
	    Document document     = getDocumentHelper();
        Element message       = document.addElement("message");
        Element event         = message.addElement("event");
        Element subscription  = event.addElement("subscription");
        subscription.addAttribute("node", node);
        event.addNamespace("", Event.NAMESPACE);
        message.addAttribute("id", request.getID());
        message.addAttribute("from", subscribingJid.toBareJID());
        message.addAttribute("subscriptions", "subscribed");
        Message rootElement = new Message(message);
        
		while (true == subscribers.hasNext()) {
			String subscriber = subscribers.next().getBareJID();
			message.addAttribute("to", subscriber);
            Message notification = rootElement.createCopy();
			outQueue.put(notification);
		}
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