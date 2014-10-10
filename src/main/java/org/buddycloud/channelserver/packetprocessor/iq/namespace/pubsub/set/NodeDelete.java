package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class NodeDelete extends PubSubElementProcessorAbstract {

    private static final String NODE_REG_EX = "^/user/[^@]+@[^/]+/[^/]+$";
	private ResultSet<NodeSubscription> subscriptions;

    public NodeDelete(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);
    }

    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {

        this.element = elm;
        this.response = IQ.createResultIQ(reqIQ);
        this.request = reqIQ;
        this.actor = actorJID;
        this.node = element.attributeValue("node");

        if (actorJID == null) {
            actor = request.getFrom();
        }
        if (!validateNode()) {
            outQueue.put(response);
            return;
        }
        if (!channelManager.isLocalNode(node)) {
            makeRemoteRequest();
            return;
        }
        if (!nodeExists() || !actorIsRegistered() || !nodeHandledByThisServer() || !actorAllowedToDelete()) {
            outQueue.put(response);
            return;
        }
        getNodeListeners();
        deleteNode();
        sendNotifications();
    }

    private void getNodeListeners() {
    	try {
			subscriptions = channelManager.getNodeSubscriptionListeners(node);
		} catch (NodeStoreException e) {
			subscriptions = new ResultSetImpl<NodeSubscription>(new ArrayList<NodeSubscription>());
		}
	}

	private void sendNotifications() throws NodeStoreException {
        try {
            Message notification = createNotificationMessage();
            if (subscriptions != null) {
                for (NodeSubscription subscription : subscriptions) {
                    notification.setTo(subscription.getListener().toString());
                    outQueue.put(notification.createCopy());
                }
            }
            Collection<JID> admins = getAdminUsers();
            for (JID admin : admins) {
                notification.setTo(admin);
                outQueue.put(notification.createCopy());
            }
        } catch (Exception e) {
            logger.error(e);
        }
    }

    private Message createNotificationMessage() {
        Message notification = new Message();
        notification.setType(Message.Type.headline);
        notification.getElement().addAttribute("remote-server-discover", "false");
        Element eventEl = notification.addChildElement("event", JabberPubsub.NS_PUBSUB_EVENT);
        Element deleteEl = eventEl.addElement("delete");
        deleteEl.addAttribute("node", node);
        return notification;
    }

    private void deleteNode() throws InterruptedException {
        try {
            channelManager.deleteNode(node);
        } catch (NodeStoreException e) {
            logger.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
            outQueue.put(response);
            return;
        }
        response.setType(IQ.Type.result);
        outQueue.put(response);
    }

    public boolean accept(Element elm) {
        return elm.getName().equals("delete");
    }

    private boolean validateNode() {
        if (node != null && !node.trim().equals("")) {
            return true;
        }
        response.setType(IQ.Type.error);
        Element nodeIdRequired = new DOMElement("nodeid-required", new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
        Element badRequest = new DOMElement(PacketError.Condition.bad_request.toXMPP(), new Namespace("", JabberPubsub.NS_XMPP_STANZAS));
        Element error = new DOMElement("error");
        error.addAttribute("type", "modify");
        error.add(badRequest);
        error.add(nodeIdRequired);
        response.setChildElement(error);
        return false;
    }

    private boolean nodeExists() throws NodeStoreException {
        if (channelManager.nodeExists(node)) {
            return true;
        }
        setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
        return false;
    }

    private boolean actorIsRegistered() {
        if (actor.getDomain().equals(getServerDomain())) {
            return true;
        }
        setErrorCondition(PacketError.Type.auth, PacketError.Condition.forbidden);
        return false;
    }

    private boolean actorAllowedToDelete() throws NodeStoreException {
        boolean isOwner = channelManager.getNodeMembership(node, actor).getAffiliation().equals(Affiliations.owner);

        if (isOwner) {
            return true;
        }
        setErrorCondition(PacketError.Type.auth, PacketError.Condition.not_authorized);
        return false;
    }

    private boolean nodeHandledByThisServer() {
        if (!node.matches(NODE_REG_EX)) {
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
            return false;
        }

        if (!node.contains("@" + getServerDomain()) && !node.contains("@" + getTopicsDomain())) {
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.not_acceptable);
            return false;
        }
        return true;
    }

    private void makeRemoteRequest() throws InterruptedException {
        request.setTo(new JID(node.split("/")[2]).getDomain());
        Element actor = request.getElement().element("pubsub").addElement("actor", Buddycloud.NS);
        actor.addText(request.getFrom().toBareJID());
        outQueue.put(request);
    }
}
