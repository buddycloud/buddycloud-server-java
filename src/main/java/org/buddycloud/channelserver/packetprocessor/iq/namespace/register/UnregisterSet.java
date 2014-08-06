package org.buddycloud.channelserver.packetprocessor.iq.namespace.register;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.NodeStore.Transaction;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.event.Event;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.resultsetmanagement.ResultSet;

public class UnregisterSet implements PacketProcessor<IQ> {
	public static final String ELEMENT_NAME = "query";
	private static final Logger LOGGER = Logger.getLogger(UnregisterSet.class);

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;

	public UnregisterSet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(IQ request) throws Exception {
		JID actorJID = null;
		Element removeEl = request.getElement().element(
				"query").element("remove");
        Element actorEl = removeEl.element("actor");
        boolean isRemote = actorEl != null;
		
        if (isRemote) {
        	actorJID = new JID(actorEl.getTextTrim());
        	if (!checkDomain(request, actorJID)) {
        		return;
        	}
        } else {
        	actorJID = new JID(request.getFrom().toBareJID());
        }
		
		unregister(request, actorJID, isRemote);
	}

	private boolean checkDomain(IQ request, JID actorJID) throws InterruptedException {
		if(!request.getFrom().getDomain().contains(actorJID.getDomain())) {
            fail(request, org.xmpp.packet.PacketError.Condition.bad_request, 
                    org.xmpp.packet.PacketError.Type.cancel);
            return false;
        }
		return true;
	}

	private void unregister(IQ request, JID actorJID, boolean isRemote) throws Exception {
		LOGGER.debug("Processing unregister request from " + request.getFrom());
		
		if (!validateSingleChildElement(request)) {
			failBadRequest(request);
			return;
		}
		if (!isRemote && !userRegistered(actorJID)) {
			failRegistrationRequired(request);
			return;
		}
		
		Transaction t = null;
		try {
			t = channelManager.beginTransaction();
			
			List<Packet> notifications = new LinkedList<Packet>();
			Set<String> remoteDomains = getRemoteDomains();
			
			ResultSet<NodeMembership> userMemberships = channelManager.getUserMemberships(actorJID);
			for (NodeMembership userMembership : userMemberships) {
				String nodeId = userMembership.getNodeId();
				if (isPersonal(nodeId) || isSingleOwner(nodeId, actorJID)) {
					channelManager.deleteNode(nodeId);
					if (channelManager.isLocalNode(nodeId)) {
						addDeleteNodeNotifications(nodeId, notifications);
					}
				}
				if (!isRemote) {
					addUnsubscribeFromNodeNotifications(actorJID, 
							userMembership.getNodeId(), notifications);
				}
			}
			
			ResultSet<NodeItem> userItems = channelManager.getUserPublishedItems(actorJID);
			for (NodeItem userItem : userItems) {
				if (channelManager.isLocalNode(userItem.getNodeId())) {
					addDeleteItemNotifications(userItem.getNodeId(), userItem.getId(), notifications);
				}
			}
			
			channelManager.deleteUserItems(actorJID);
			channelManager.deleteUserSubscriptions(actorJID);
			channelManager.deleteUserAffiliations(actorJID);
			
			outQueue.put(IQ.createResultIQ(request));
			if (!isRemote) {
				makeRemoteRequests(request, remoteDomains);
			}
			sendNotifications(notifications);
			
			t.commit();
		} finally {
			if (t != null) {
				t.close();
			}
		}
	}
	
	private void sendNotifications(List<Packet> notifications) throws InterruptedException {
		for (Packet notification : notifications) {
			outQueue.put(notification);
		}
	}

	private void makeRemoteRequests(IQ request, Set<String> remoteDomains) throws Exception {
		for (String remoteDomain : remoteDomains) {
			IQ remoteRequest = request.createCopy();
			remoteRequest.getElement().addAttribute("remote-server-discover", "false");
			remoteRequest.setTo(remoteDomain);
			Element actor = remoteRequest.getElement()
					.element("query").element("remove")
					.addElement("actor", Buddycloud.NS);
			actor.addText(request.getFrom().toBareJID());
			outQueue.put(remoteRequest);
		}
	}

	private Set<String> getRemoteDomains() throws NodeStoreException {
		ArrayList<String> nodeList = channelManager.getNodeList();
		Set<String> remoteDomains = new HashSet<String>();
		for (String node : nodeList) {
			try {
				if (!channelManager.isLocalNode(node)) {
					remoteDomains.add(new JID(node.split("/")[2]).getDomain());
				}
			} catch (IllegalArgumentException e) {
				// Ignore bad formatted nodes
			}
		}
		return remoteDomains;
	}

	private void addUnsubscribeFromNodeNotifications(JID userJid,
			String node, List<Packet> notifications) throws NodeStoreException {

		ResultSet<NodeSubscription> subscribers = channelManager
				.getNodeSubscriptionListeners(node);

		Message messagePacket = createUnsubscribeNotification(userJid, node);
		if (subscribers != null) {
			for (NodeSubscription subscriber : subscribers) {
				Message notification = messagePacket.createCopy();
				notification.setTo(subscriber.getListener());
				notifications.add(notification);
			}
		}
		
		Collection<JID> adminUsers = Configuration.getInstance()
				.getAdminUsers();
		for (JID admin : adminUsers) {
			Message notification = messagePacket.createCopy();
			notification.setTo(admin);
			notifications.add(notification);
		}
	}

	private Message createUnsubscribeNotification(JID userJid, String node) {
		Message messagePacket = new Message();
		Element messageEl = messagePacket.getElement();
		messageEl.addAttribute("remote-server-discover", "false");
		Element event = messageEl.addElement("event", Event.NAMESPACE);
		Element subscription = event.addElement("subscription");
		messageEl.addAttribute("type", "headline");
		subscription.addAttribute("subscription", "none");
		subscription.addAttribute("jid", userJid.toBareJID());
		subscription.addAttribute("node", node);
		return messagePacket;
	}

	private void addDeleteItemNotifications(String node, String itemId, 
			List<Packet> notifications) throws NodeStoreException {
		ResultSet<NodeSubscription> subscriptions = channelManager
				.getNodeSubscriptionListeners(node);
		Message notification = createItemDeleteNotificationMessage(node, itemId);
		if (subscriptions != null) {
			for (NodeSubscription subscription : subscriptions) {
				if (subscription.getSubscription().equals(Subscriptions.subscribed)) {
					notification.setTo(subscription.getListener().toString());
					notifications.add(notification);
				}
			}
		}
		
		Collection<JID> adminUsers = Configuration.getInstance()
				.getAdminUsers();
		for (JID admin : adminUsers) {
			notification.setTo(admin);
			notifications.add(notification);
		}
	}
	
	private Message createItemDeleteNotificationMessage(String node, String itemId) {
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

	private void addDeleteNodeNotifications(String node, List<Packet> notifications) throws Exception {
		ResultSet<NodeSubscription> subscriptions = channelManager
				.getNodeSubscriptionListeners(node);
		Message notification = createNodeDeletedNotificationMessage(node);
		if (subscriptions != null) {
			for (NodeSubscription subscription : subscriptions) {
				notification.setTo(subscription.getListener().toString());
				notifications.add(notification.createCopy());
			}
		}
		Collection<JID> adminUsers = Configuration.getInstance()
				.getAdminUsers();
		for (JID admin : adminUsers) {
			notification.setTo(admin);
			notifications.add(notification.createCopy());
		}
	}

	private Message createNodeDeletedNotificationMessage(String node) {
		Message notification = new Message();
		notification.setType(Message.Type.headline);
		notification.getElement().addAttribute("remote-server-discover", "false");
		Element eventEl = notification.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		Element deleteEl = eventEl.addElement("delete");
		deleteEl.addAttribute("node", node);
		return notification;
	}
	
	
	private boolean isSingleOwner(String nodeId, JID userJid) throws NodeStoreException {
		ResultSet<NodeMembership> nodeMemberships = channelManager.getNodeMemberships(nodeId);
		int ownerCount = 0;
		boolean isOwner = false;
		for (NodeMembership nodeMembership : nodeMemberships) {
			if (nodeMembership.getAffiliation().equals(Affiliations.owner)) {
				ownerCount++;
				if (nodeMembership.getUser().equals(userJid)) {
					isOwner = true;
				}
			}
		}
		return isOwner && ownerCount == 1;
	}

	private boolean isPersonal(String nodeId) throws NodeStoreException {
		String channelType = channelManager.getNodeConfValue(nodeId, Conf.CHANNEL_TYPE);
		return channelType != null && channelType.equals("personal"); 
	}

	private boolean validateSingleChildElement(IQ request) {
		return request.getElement().element("query").elements().size() == 1;
	}
	
	private void failBadRequest(IQ request) throws InterruptedException {
		fail(request,
				org.xmpp.packet.PacketError.Condition.bad_request,
				org.xmpp.packet.PacketError.Type.modify);
	}

	private void failRegistrationRequired(IQ request) throws InterruptedException {
		fail(request,
				org.xmpp.packet.PacketError.Condition.registration_required,
				org.xmpp.packet.PacketError.Type.auth);
	}
	
	private void fail(IQ request, Condition condition, org.xmpp.packet.PacketError.Type type) throws InterruptedException {
		IQ reply = IQ.createResultIQ(request);
		reply.setType(Type.error);
		PacketError pe = new PacketError(condition, type);
		reply.setError(pe);
		outQueue.put(reply);
	}

	private boolean userRegistered(JID actorJID) throws Exception {
		return channelManager.nodeExists("/user/" + actorJID.toBareJID() + "/posts");
	}

}