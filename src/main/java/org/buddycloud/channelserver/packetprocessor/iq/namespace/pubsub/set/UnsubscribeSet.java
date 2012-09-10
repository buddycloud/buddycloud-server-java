package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.Iterator;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.event.Event;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.queue.statemachine.Unsubscribe;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class UnsubscribeSet extends PubSubElementProcessorAbstract {

	private final BlockingQueue<Packet> outQueue;
	private final DataStore dataStore;

	private String node;
	private IQ     request;
	private JID    unsubscribingJid;

	public UnsubscribeSet(BlockingQueue<Packet> outQueue, DataStore dataStore) {
		this.outQueue = outQueue;
		this.dataStore = dataStore;
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

	    unsubscribingJid          = request.getFrom();
		boolean isLocalNode       = dataStore.isLocalNode(node);
		boolean isLocalSubscriber = false;

		if (actorJID != null) {
			unsubscribingJid = actorJID;
		} else {
			isLocalSubscriber = dataStore.isLocalUser(unsubscribingJid
					.toBareJID());
			// Check that user is registered.
			if (!isLocalSubscriber) {
				failAuthRequired();
				return;

			}
		}

		if (!isLocalNode) {

			if (isLocalSubscriber) {
				// Start process to unsubscribe from external node.
				Unsubscribe unsub = Unsubscribe.buildUnsubscribeStatemachine(
						node, request, dataStore);
				outQueue.put(unsub.nextStep());
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
					org.xmpp.packet.PacketError.Condition.item_not_found,
					org.xmpp.packet.PacketError.Type.cancel);
			reply.setError(pe);
			outQueue.put(reply);
			return;
		}

		dataStore.unsubscribeUserFromNode(unsubscribingJid.toBareJID(), node);

		IQ reply = IQ.createResultIQ(request);
		outQueue.put(reply);
		notifySubscribers();
	}

	private void notifySubscribers() throws DataStoreException, InterruptedException
	{
	    Iterator<? extends NodeSubscription> subscribers = dataStore.getNodeSubscribers(node);
	    Document document     = getDocumentHelper();
        Element message       = document.addElement("message");
        Element event         = message.addElement("event");
        Element subscription  = event.addElement("subscription");
        Element affiliation   = event.addElement("affiliation");
        subscription.addAttribute("node", node);
        subscription.addAttribute("jid", unsubscribingJid.toBareJID());
        subscription.addAttribute("subscription", "none");
        event.addNamespace("", Event.NAMESPACE);
        message.addAttribute("id", request.getID());
        message.addAttribute("from", unsubscribingJid.toBareJID());
        affiliation.addAttribute("affiliation", "none");
        affiliation.addAttribute("jid", unsubscribingJid.toBareJID());
        affiliation.addAttribute("node", node);

        Message rootElement = new Message(message);
        
		while (true == subscribers.hasNext()) {
			String subscriber = subscribers.next().getBareJID();
			message.addAttribute("to", subscriber);
            Message notification = rootElement.createCopy();
			outQueue.put(notification);
		}
	}
	
	private void failAuthRequired() throws InterruptedException {
		// If the packet did not have actor, and the sender is not a
		// local user
		// subscription is not allowed.

		/*
		 * <iq type='error' from='pubsub.shakespeare.lit'
		 * to='hamlet@denmark.lit/elsinore' id='unsub1'> <error
		 * type='auth'> <registration-required
		 * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> </error> </iq>
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
		return elm.getName().equals("unsubscribe");
	}
}