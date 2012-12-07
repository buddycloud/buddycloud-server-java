package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.io.StringReader;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class ItemDelete extends PubSubElementProcessorAbstract {

	private static final Logger LOGGER = Logger.getLogger(ItemDelete.class);

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;

	private String itemId;
	private NodeItem nodeItem;
	private Element parsedPayload;

	public ItemDelete(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws InterruptedException, NodeStoreException {

		element = elm;
		request = reqIQ;
		response = IQ.createResultIQ(request);
		node    = element.attributeValue("node");
		
		if (false == channelManager.isLocalNode(node)) {
			makeRemoteRequest();
			return;
		}

		try {
			if ((false == validNodeProvided()) || (false == nodeExists())
					|| (false == itemIdProvided()) || (false == itemExists())
					|| (false == validPayload()) || (false == canDelete())) {
				outQueue.put(response);
				return;
			}
			deleteItem();
			outQueue.put(response);
			sendNotifications();
			return;
		} catch (NodeStoreException e) {
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
		} catch (NullPointerException e) {
			setErrorCondition(PacketError.Type.modify,
					PacketError.Condition.bad_request);
		} catch (IllegalArgumentException e) {
			setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
		}
		outQueue.put(response);
	}

	private void sendNotifications() throws NodeStoreException {
		try {
			String notify = request.getElement().element("pubsub")
					.element("retract").attributeValue("notify");

			if ((null == notify) || ((false == notify.equals("true"))
					&& (false == notify.equals("1")))) {
				return;
			}
			ResultSet<NodeSubscription> subscriptions = channelManager
					.getNodeSubscriptionListeners(node);
			Message notification = getNotificationMessage();
			for (NodeSubscription subscription : subscriptions) {
				if (subscription.getSubscription().equals(
						Subscriptions.subscribed)) {
					notification.setTo(subscription.getListener().toString());
					outQueue.put(notification.createCopy());
				}
			}
		} catch (NullPointerException e) {
			return;
		} catch (InterruptedException e) {
			return;
		}
	}

	private Message getNotificationMessage() {
		Message notification = new Message();
		notification.setType(Message.Type.headline);
		notification.setID(request.getID() + "-1");
		Element event = notification.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		Element items = event.addElement("items");
		items.addAttribute("node", node);
		Element retract = items.addElement("retract");
		retract.addAttribute("id", itemId);
		return notification;
	}

	private void deleteItem() throws NodeStoreException {
		channelManager.deleteNodeItemById(node, itemId);
	}

	private boolean canDelete() throws NodeStoreException {
		if ((false == userOwnsItem()) && (false == userManagesNode())) {
			setErrorCondition(PacketError.Type.auth,
					PacketError.Condition.forbidden);
			return false;
		}
		return true;
	}

	private boolean userOwnsItem() {
		try {
			return parsedPayload.element("author").elementText("name")
					.equals(request.getFrom().toBareJID());
		} catch (NullPointerException e) {
			return false;
		}
	}

	private boolean userManagesNode() throws NodeStoreException {
		NodeAffiliation affiliation = channelManager.getUserAffiliation(node,
				new JID(request.getFrom().toBareJID()));
		if (null == affiliation) {
			return false;
		}
		return affiliation.getAffiliation().in(Affiliations.owner,
				Affiliations.moderator);
	}

	private boolean validPayload() {

		try {
			SAXReader xmlReader = new SAXReader();
			xmlReader.setMergeAdjacentText(true);
			xmlReader.setStringInternEnabled(true);
			xmlReader.setStripWhitespaceText(true);
			parsedPayload = xmlReader.read(
					new StringReader(nodeItem.getPayload())).getRootElement();
			return true;
		} catch (Exception e) {
			LOGGER.error(e);
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
			return false;
		}
	}

	private boolean itemExists() throws NodeStoreException {
		nodeItem = channelManager.getNodeItem(node, itemId);
		if (null != nodeItem) {
			return true;
		}
		setErrorCondition(PacketError.Type.cancel,
				PacketError.Condition.item_not_found);
		return false;
	}

	private boolean itemIdProvided() {
		itemId = request.getElement().element("pubsub").element("retract")
				.element("item").attributeValue("id");
		if ((null != itemId) && (false == itemId.equals(""))) {
			return true;
		}
		response.setType(IQ.Type.error);
		Element nodeIdRequired = new DOMElement("item-required", new Namespace(
				"", JabberPubsub.NS_PUBSUB_ERROR));
		Element badRequest = new DOMElement(
				PacketError.Condition.bad_request.toXMPP(), new Namespace("",
						JabberPubsub.NS_XMPP_STANZAS));
		Element error = new DOMElement("error");
		error.addAttribute("type", PacketError.Type.modify.toXMPP());
		error.add(badRequest);
		error.add(nodeIdRequired);
		response.setChildElement(error);
		return false;
	}

	private boolean nodeExists() throws NodeStoreException {
		if ((false == channelManager.isLocalNode(node))
				|| (false == channelManager.nodeExists(node))) {
			setErrorCondition(PacketError.Type.cancel,
					PacketError.Condition.item_not_found);
			return false;
		}
		return true;
	}

	private boolean validNodeProvided() {
		if ((null != node) && (false == node.equals(""))) {
			return true;
		}
		response.setType(IQ.Type.error);
		Element nodeIdRequired = new DOMElement("nodeid-required",
				new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
		Element badRequest = new DOMElement(
				PacketError.Condition.bad_request.toXMPP(), new Namespace("",
						JabberPubsub.NS_XMPP_STANZAS));
		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		error.add(badRequest);
		error.add(nodeIdRequired);
		response.setChildElement(error);
		return false;
	}
	
	private void makeRemoteRequest() throws InterruptedException {
		request.setTo(new JID(node.split("/")[2]).getDomain());
		Element actor = request.getElement()
		    .element("pubsub")
		    .addElement("actor", JabberPubsub.NS_BUDDYCLOUD);
		actor.addText(request.getFrom().toBareJID());
	    outQueue.put(request);
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("retract");
	}
}