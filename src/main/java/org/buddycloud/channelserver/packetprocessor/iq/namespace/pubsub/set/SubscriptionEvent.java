package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscriptionEvent extends PubSubElementProcessorAbstract {

    Element subscriptionElement;
    NodeMembership currentMembership;
    JID invitedBy = null;
    private Subscriptions requestedSubscription;
    private JID jid;

    private static final Logger LOGGER = Logger.getLogger(SubscriptionEvent.class);

    public static final String CAN_NOT_MODIFY_OWN_SUBSCRIPTION = "can-not-modify-own-subscription";

    /**
     * Constructor
     * 
     * @param outQueue Outgoing message queue
     * @param channelManager Data Access Object (DAO)
     */
    public SubscriptionEvent(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);
    }

    /**
     * Process incoming stanza
     */
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        element = elm;
        response = IQ.createResultIQ(reqIQ);
        request = reqIQ;
        actor = actorJID;
        node = element.attributeValue("node");

        if (actor == null) {
            actor = request.getFrom();
        }
        if (false == channelManager.isLocalNode(node)) {
            makeRemoteRequest();
            return;
        }
        try {
            if ((false == nodeProvided()) || (false == validRequestStanza()) || (false == checkNodeExists()) || (false == actorHasPermissionToAuthorize())
                    || (true == actorIsModifyingTheirSubscription()) || (false == userIsSubscribable())) {
                outQueue.put(response);
                return;
            }
            saveUpdatedSubscription();
            sendNotifications();
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
            outQueue.put(response);
            return;
        }
    }

    private boolean actorIsModifyingTheirSubscription() {
        if (actor.toBareJID().equals(subscriptionElement.attributeValue("jid"))) {
            createExtendedErrorReply(PacketError.Type.cancel, PacketError.Condition.not_allowed, CAN_NOT_MODIFY_OWN_SUBSCRIPTION, Buddycloud.NS_ERROR);
            return true;
        }
        return false;
    }

    private void sendNotifications() throws Exception {

        boolean hasNotifiedInviteesServer = false;
        JID invitedUsersDomain = new JID(jid.getDomain());

        outQueue.put(response);

        ResultSet<NodeSubscription> subscribers = channelManager.getNodeSubscriptionListeners(node);

        Document document = getDocumentHelper();
        Element message = document.addElement("message");
        message.addAttribute("type", "headline");
        message.addAttribute("remote-server-discover", "false");
        Element event = message.addElement("event", JabberPubsub.NS_PUBSUB_EVENT);
        Element subscription = event.addElement("subscription");
        message.addAttribute("from", request.getTo().toString());
        subscription.addAttribute("node", node);
        subscription.addAttribute("jid", subscriptionElement.attributeValue("jid"));
        subscription.addAttribute("subscription", subscriptionElement.attributeValue("subscription"));
        Message rootElement = new Message(message);

        Subscriptions newSubscription = Subscriptions.valueOf(subscriptionElement.attributeValue("subscription"));

        if (newSubscription.equals(Subscriptions.invited)) {
            subscription.addAttribute("invited-by", actor.toBareJID());
        }

        for (NodeSubscription subscriber : subscribers) {
            Message notification = rootElement.createCopy();
            notification.setTo(subscriber.getListener());
            if (subscriber.getUser().getDomain().equals(invitedUsersDomain.getDomain())) {
                hasNotifiedInviteesServer = true;
            }
            outQueue.put(notification);
        }
        Collection<JID> admins = getAdminUsers();
        for (JID admin : admins) {
            Message notification = rootElement.createCopy();
            notification.setTo(admin);
            outQueue.put(notification);
        }

        if (hasNotifiedInviteesServer) {
            return;
        }

        if (newSubscription.equals(Subscriptions.invited)) {
            Message alertInvitedUser = rootElement.createCopy();
            JID to = jid;
            alertInvitedUser.getElement().attribute("remote-server-discover").detach();
            if (!channelManager.isLocalJID(jid)) {
                to = invitedUsersDomain;
            }
            alertInvitedUser.setTo(to);
            outQueue.put(alertInvitedUser);
        }
    }

    private void saveUpdatedSubscription() throws NodeStoreException {
        if (requestedSubscription.equals(Subscriptions.invited)) {
            invitedBy = actor;
        }
        jid = new JID(subscriptionElement.attributeValue("jid"));
        NodeSubscription newSubscription =
                new NodeSubscriptionImpl(node, jid, currentMembership.getListener(), Subscriptions.valueOf(subscriptionElement
                        .attributeValue("subscription")), invitedBy);

        channelManager.addUserSubscription(newSubscription);
    }

    private boolean nodeProvided() {
        if (null != node) {
            return true;
        }
        response.setType(IQ.Type.error);
        Element nodeIdRequired = new DOMElement("nodeid-required", new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
        Element badRequest = new DOMElement(PacketError.Condition.bad_request.toString(), new Namespace("", JabberPubsub.NS_XMPP_STANZAS));
        Element error = new DOMElement("error");
        error.addAttribute("type", "modify");
        error.add(badRequest);
        error.add(nodeIdRequired);
        response.setChildElement(error);
        return false;
    }

    private boolean validRequestStanza() {
        try {
            subscriptionElement = request.getElement().element("pubsub").element("subscriptions").element("subscription");
            requestedSubscription = Subscriptions.createFromString(subscriptionElement.attributeValue("subscription"));

            if ((null == subscriptionElement) || (null == subscriptionElement.attribute("jid")) || (null == subscriptionElement.attribute("subscription"))) {
                setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
                return false;
            }
        } catch (NullPointerException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
            return false;
        }
        subscriptionElement.addAttribute("subscription", requestedSubscription.toString());
        return true;
    }

    private boolean userIsSubscribable() throws NodeStoreException {
        currentMembership = channelManager.getNodeMembership(node, new JID(subscriptionElement.attributeValue("jid")));
        if (currentMembership.getSubscription().equals(Subscriptions.none) && requestedSubscription.equals(Subscriptions.invited)) {
            return true;
        }
        if (!currentMembership.getSubscription().equals(Subscriptions.none)) {
            return true;
        }
        setErrorCondition(PacketError.Type.modify, PacketError.Condition.unexpected_request);
        return false;
    }

    private boolean actorHasPermissionToAuthorize() throws NodeStoreException {
        NodeMembership membership = channelManager.getNodeMembership(node, actor);

        if (membership.getAffiliation().canAuthorize()) {
            return true;
        }
        if (membership.getSubscription().equals(Subscriptions.subscribed) && requestedSubscription.equals(Subscriptions.invited)) {
            return true;
        }

        setErrorCondition(PacketError.Type.auth, PacketError.Condition.forbidden);
        return false;
    }

    private boolean checkNodeExists() throws NodeStoreException {
        if (false == channelManager.nodeExists(node)) {
            setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
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

    /**
     * Determine if this class is capable of processing incoming stanza
     */
    public boolean accept(Element elm) {
        return elm.getName().equals("subscriptions");
    }
}
