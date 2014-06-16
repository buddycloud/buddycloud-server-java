package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.io.StringReader;
import java.util.Collection;
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
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
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
	public void process(Element elm, JID actor, IQ reqIQ, Element rsm)
			throws InterruptedException, NodeStoreException {

		element = elm;
		request = reqIQ;
		response = IQ.createResultIQ(request);
		node = element.attributeValue("node");
		this.actor = actor;
		if (null == this.actor) this.actor = request.getFrom();
		
		if (!channelManager.isLocalNode(node)) {
			makeRemoteRequest();
			return;
		}

		try {
			if (!validNodeProvided() || !nodeExists() || !itemIdProvided() 
					|| !itemExists() || !validPayload() || !canDelete()) {
				outQueue.put(response);
				return;
			}
			deleteItem();
			outQueue.put(response);
			sendNotifications();
			return;
		} catch (NodeStoreException e) {
			logger.error(e);
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
		} catch (NullPointerException e) {
			logger.error(e);
			setErrorCondition(PacketError.Type.modify,
					PacketError.Condition.bad_request);
		} catch (IllegalArgumentException e) {
			logger.error(e);
			setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
		}
		outQueue.put(response);
	}

	private void sendNotifications() throws NodeStoreException {
		try {
			String notify = request.getElement().element("pubsub")
					.element("retract").attributeValue("notify");

			if ((notify != null) && (notify.equals("false") || notify.equals("0"))) {
				return;
			}
			ResultSet<NodeSubscription> subscriptions = channelManager
					.getNodeSubscriptionListeners(node);
			Message notification = getNotificationMessage();

			for (NodeSubscription subscription : subscriptions) {
				logger.debug("Subscription [node: " + subscription.getNodeId() + ", listener: " 
						+ subscription.getListener() + ", subscription: " + subscription.getSubscription() + "]");
				if (subscription.getSubscription().equals(
						Subscriptions.subscribed)) {
					notification.setTo(subscription.getListener());
					outQueue.put(notification.createCopy());
				}
			}
			
			Collection<JID> admins = getAdminUsers();
			for (JID admin : admins) {
				notification.setTo(admin);
				outQueue.put(notification.createCopy());
			}
		} catch (NullPointerException e) {
			logger.error(e);
			return;
		} catch (InterruptedException e) {
			logger.error(e);
			return;
		}
	}

	private Message getNotificationMessage() {
		Message notification = new Message();
		notification.setType(Message.Type.headline);
		notification.getElement().addAttribute("remote-server-discover", "false");
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
		if (!userOwnsItem() && !userManagesNode()) {
			setErrorCondition(PacketError.Type.auth,
					PacketError.Condition.forbidden);
			return false;
		}
		return true;
	}

	private boolean userOwnsItem() {
		try {
			return parsedPayload.element("author").elementText("name")
					.equals(actor.toBareJID());
		} catch (NullPointerException e) {
			return false;
		}
	}

	private boolean userManagesNode() throws NodeStoreException {
		return channelManager.getNodeMembership(node,
				actor).getAffiliation().canAuthorize();
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
		if (nodeItem != null) {
			return true;
		}
		setErrorCondition(PacketError.Type.cancel,
				PacketError.Condition.item_not_found);
		return false;
	}

	private boolean itemIdProvided() {
		itemId = request.getElement().element("pubsub").element("retract")
				.element("item").attributeValue("id");
		if (itemId != null && !itemId.equals("")) {
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
		if (node != null && !node.equals("")) {
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
		request.getElement()
		    .element("pubsub")
		    .addElement("actor", Buddycloud.NS)
            .addText(actor.toBareJID());
	    outQueue.put(request);
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("retract");
	}
}