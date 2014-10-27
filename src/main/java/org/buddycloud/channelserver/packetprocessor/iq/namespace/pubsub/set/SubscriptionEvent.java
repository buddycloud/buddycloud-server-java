package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Document;
import org.dom4j.Element;
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

        acceptedElementName = XMLConstants.SUBSCRIPTIONS_ELEM;


    }

    /**
     * Process incoming stanza
     */
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        element = elm;
        response = IQ.createResultIQ(reqIQ);
        request = reqIQ;
        actor = actorJID;
        node = element.attributeValue(XMLConstants.NODE_ATTR);

        if (actor == null) {
            actor = request.getFrom();
        }

        if (!nodePresent()) {
            outQueue.put(response);
            return;
        }

        if (!Configuration.getInstance().isLocalNode(node)) {
            makeRemoteRequest();
            return;
        }
        try {
            if ((!isValidStanza()) || (!checkNodeExists()) || (!actorHasPermissionToAuthorize()) || (actorIsModifyingTheirSubscription())
                    || (!userIsSubscribable())) {
                outQueue.put(response);
                return;
            }
            saveUpdatedSubscription();
            sendNotifications();
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
            outQueue.put(response);
        }

        return;
    }

    private boolean actorIsModifyingTheirSubscription() {
        if (actor.toBareJID().equals(subscriptionElement.attributeValue(XMLConstants.JID_ATTR))) {
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
        Element message = document.addElement(XMLConstants.MESSAGE_ELEM);
        message.addAttribute(XMLConstants.TYPE_ATTR, "headline");
        message.addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR, Boolean.FALSE.toString());
        Element event = message.addElement(XMLConstants.EVENT_ELEM, JabberPubsub.NS_PUBSUB_EVENT);
        Element subscription = event.addElement(XMLConstants.SUBSCRIPTION_ELEM);
        message.addAttribute(XMLConstants.FROM_ATTR, request.getTo().toString());
        subscription.addAttribute(XMLConstants.NODE_ATTR, node);
        subscription.addAttribute(XMLConstants.JID_ATTR, subscriptionElement.attributeValue(XMLConstants.JID_ATTR));
        subscription.addAttribute(XMLConstants.SUBSCRIPTION_ELEM, subscriptionElement.attributeValue(XMLConstants.SUBSCRIPTION_ELEM));
        Message rootElement = new Message(message);

        Subscriptions newSubscription = Subscriptions.valueOf(subscriptionElement.attributeValue(XMLConstants.SUBSCRIPTION_ELEM));

        if (newSubscription.equals(Subscriptions.invited)) {
            subscription.addAttribute(XMLConstants.INVITED_BY_ATTR, actor.toBareJID());
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
            alertInvitedUser.getElement().attribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR).detach();
            if (!Configuration.getInstance().isLocalJID(jid)) {
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
        jid = new JID(subscriptionElement.attributeValue(XMLConstants.JID_ATTR));
        NodeSubscription newSubscription =
                new NodeSubscriptionImpl(node, jid, currentMembership.getListener(), Subscriptions.valueOf(subscriptionElement
                        .attributeValue(XMLConstants.SUBSCRIPTION_ATTR)), invitedBy);

        channelManager.addUserSubscription(newSubscription);
    }

    protected boolean isValidStanza() {
        subscriptionElement =
                request.getElement().element(XMLConstants.PUBSUB_ELEM).element(XMLConstants.SUBSCRIPTIONS_ELEM).element(XMLConstants.SUBSCRIPTION_ELEM);


        if ((null == subscriptionElement) || (null == subscriptionElement.attribute(XMLConstants.JID_ATTR))
                || (null == subscriptionElement.attribute(XMLConstants.SUBSCRIPTION_ATTR))) {
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
            return false;
        }

        requestedSubscription = Subscriptions.createFromString(subscriptionElement.attributeValue(XMLConstants.SUBSCRIPTION_ELEM));
        subscriptionElement.addAttribute(XMLConstants.SUBSCRIPTION_ATTR, requestedSubscription.toString());
        return true;
    }

    private boolean userIsSubscribable() throws NodeStoreException {
        currentMembership = channelManager.getNodeMembership(node, new JID(subscriptionElement.attributeValue(XMLConstants.JID_ATTR)));
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
}
