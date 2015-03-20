package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.io.StringReader;
import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
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

    private GlobalItemID itemId;
    private NodeItem nodeItem;
    private Element parsedPayload;

    public ItemDelete(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        this.setOutQueue(outQueue);
        this.setChannelManager(channelManager);

        acceptedElementName = "retract";
    }

    @Override
    public void process(Element elm, JID actor, IQ reqIQ, Element rsm) throws InterruptedException, NodeStoreException {

        element = elm;
        request = reqIQ;
        response = IQ.createResultIQ(request);
        node = element.attributeValue(XMLConstants.NODE_ATTR);
        this.actor = actor;
        if (null == this.actor) {
            this.actor = request.getFrom();
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
            if (!checkNodeExists() || !itemIdProvided() || !itemExists() || !validPayload() || !canDelete()) {
                outQueue.put(response);
                return;
            }
            deleteItem();
            outQueue.put(response);
            deleteReplies();
            sendNotifications(node, itemId);
            return;
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
        } catch (NullPointerException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
        } catch (IllegalArgumentException e) {
            LOGGER.error(e);
            createExtendedErrorReply(
                PacketError.Type.modify, PacketError.Condition.bad_request,
                "global-id-error", Buddycloud.NS_ERROR, e.getMessage()
            );
        }
        outQueue.put(response);
    }

    private void deleteReplies() throws NodeStoreException {
        if (null != nodeItem.getInReplyTo()) {
            return;
        }
        ClosableIteratorImpl<NodeItem> replies = channelManager.getNodeItemReplies(node, itemId.getItemID(), null, -1);
        NodeItem reply = null;
        while (replies.hasNext()) {
            reply = replies.next();
            channelManager.deleteNodeItemById(reply.getNodeId(), reply.getId());

            sendNotifications(node, new GlobalItemIDImpl(new JID(
                Configuration.getInstance().getServerDomain()), reply.getNodeId(), reply.getId()));
        }
    }

    private void sendNotifications(String node, GlobalItemID itemId) throws NodeStoreException {
        try {
            String notify =
                    request.getElement().element(XMLConstants.PUBSUB_ELEM).element(XMLConstants.RETRACT_ELEM).attributeValue(XMLConstants.NOTIFY_ATTR);

            if ((notify != null) && (Boolean.FALSE.toString().equals(notify) || notify.equals("0"))) {
                return;
            }
            ResultSet<NodeSubscription> subscriptions = channelManager.getNodeSubscriptionListeners(node);
            Message notification = getNotificationMessage(node, itemId);

            for (NodeSubscription subscription : subscriptions) {
                LOGGER.debug("Subscription [node: " + subscription.getNodeId() + ", listener: " + subscription.getListener() + ", subscription: "
                        + subscription.getSubscription() + "]");
                if (subscription.getSubscription().equals(Subscriptions.subscribed)) {

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
            LOGGER.error(e);
            return;
        } catch (InterruptedException e) {
            LOGGER.error(e);
            return;
        }
    }

    private Message getNotificationMessage(String node, GlobalItemID itemId) {
        Message notification = new Message();
        notification.setType(Message.Type.headline);
        notification.getElement().addAttribute("remote-server-discover", "false");
        Element event = notification.addChildElement("event", JabberPubsub.NS_PUBSUB_EVENT);
        Element items = event.addElement("items");
        items.addAttribute("node", node);
        Element retract = items.addElement("retract");
        retract.addAttribute("id", itemId.getItemID());
        return notification;
    }

    private void deleteItem() throws NodeStoreException {
        channelManager.deleteNodeItemById(node, itemId.getItemID());
    }

    private boolean canDelete() throws NodeStoreException {
        if (!userOwnsItem() && !userManagesNode()) {
            setErrorCondition(PacketError.Type.auth, PacketError.Condition.forbidden);
            return false;
        }
        return true;
    }

    private boolean userOwnsItem() {
        try {
            return parsedPayload.element("author").elementText("name").equals(actor.toBareJID());
        } catch (NullPointerException e) {
            return false;
        }
    }

    private boolean userManagesNode() throws NodeStoreException {
        return channelManager.getNodeMembership(node, actor).getAffiliation().canAuthorize();
    }

    private boolean validPayload() {

        try {
            SAXReader xmlReader = new SAXReader();
            xmlReader.setMergeAdjacentText(true);
            xmlReader.setStringInternEnabled(true);
            xmlReader.setStripWhitespaceText(true);
            parsedPayload = xmlReader.read(new StringReader(nodeItem.getPayload())).getRootElement();
            return true;
        } catch (Exception e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
            return false;
        }
    }

    private boolean itemExists() throws NodeStoreException {
        nodeItem = channelManager.getNodeItem(node, itemId.getItemID());
        if (nodeItem != null) {
            return true;
        }
        setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
        return false;
    }

    private boolean itemIdProvided() {
        String id =
                request.getElement().element(XMLConstants.PUBSUB_ELEM).element(XMLConstants.RETRACT_ELEM).element(XMLConstants.ITEM_ELEM)
                        .attributeValue(XMLConstants.ID_ATTR);

        if ((id != null) && !id.isEmpty()) {
            if (GlobalItemIDImpl.isGlobalId(id)) {
                itemId = GlobalItemIDImpl.fromBuddycloudString(id);
            } else {
                itemId = new GlobalItemIDImpl(new JID(
                    Configuration.getInstance().getServerDomain()), node, id);
            }
            return true;
        }
        response.setType(IQ.Type.error);
        Element nodeIdRequired = new DOMElement("item-required", new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
        Element badRequest = new DOMElement(PacketError.Condition.bad_request.toXMPP(), new Namespace("", JabberPubsub.NS_XMPP_STANZAS));
        Element error = new DOMElement("error");
        error.addAttribute("type", PacketError.Type.modify.toXMPP());
        error.add(badRequest);
        error.add(nodeIdRequired);
        response.setChildElement(error);
        return false;
    }
}
