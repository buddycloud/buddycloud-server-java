package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.XMLConstants;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscriptionsGet implements PubSubElementProcessor {

    private final BlockingQueue<Packet> outQueue;
    private ChannelManager channelManager;

    private IQ result;
    private String node;
    private JID actorJid;
    private IQ requestIq;


    public SubscriptionsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        this.outQueue = outQueue;
        this.channelManager = channelManager;
    }

    public void setChannelManager(ChannelManager dataStore) {
        channelManager = dataStore;
    }


    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        result = IQ.createResultIQ(reqIQ);
        actorJid = actorJID;
        requestIq = reqIQ;
        Element pubsub = result.setChildElement(XMLConstants.PUBSUB_ELEM, JabberPubsub.NAMESPACE_URI);
        Element subscriptions = pubsub.addElement(XMLConstants.SUBSCRIPTIONS_ELEM);

        node = reqIQ.getChildElement().element(XMLConstants.SUBSCRIPTIONS_ELEM).attributeValue(XMLConstants.NODE_ATTR);

        if (null == actorJid) {
            actorJid = reqIQ.getFrom();
        }

        boolean isProcessedLocally = true;

        if (node == null) {
            isProcessedLocally = getUserMemberships(subscriptions);
        } else {
            if (!channelManager.isLocalNode(node)) {
                result.getElement().addAttribute("remote-server-discover", "false");
            }
            isProcessedLocally = getNodeMemberships(subscriptions);
        }
        if (!isProcessedLocally) {
            return;
        }

        outQueue.put(result);
    }

    private boolean getNodeMemberships(Element subscriptions) throws NodeStoreException, InterruptedException {

        ResultSet<NodeMembership> cur = channelManager.getNodeMemberships(node);

        if (channelManager.isLocalNode(node)) {

            subscriptions.addAttribute(XMLConstants.NODE_ATTR, node);

            for (NodeMembership ns : cur) {
                if (actorJid.toBareJID().equals(ns.getUser().toBareJID())) {
                    Element subscription = subscriptions.addElement(XMLConstants.SUBSCRIPTION_ELEM);
                    subscription.addAttribute(XMLConstants.NODE_ATTR, ns.getNodeId())
                            .addAttribute(XMLConstants.SUBSCRIPTION_ELEM, ns.getSubscription().toString())
                            .addAttribute(XMLConstants.JID_ATTR, ns.getUser().toBareJID());
                    if (null != ns.getInvitedBy()) {
                        subscription.addAttribute(XMLConstants.INVITED_BY_ELEM, ns.getInvitedBy().toBareJID());
                    }
                }

            }
        } else {

            if (!channelManager.isCachedNode(node) || (null != requestIq.getElement().element(XMLConstants.PUBSUB_ELEM).element(XMLConstants.SET_ELEM))
                    && !cur.isEmpty()) {
                makeRemoteRequest(new JID(node.split("/")[2]).getDomain());
            }
            return false;
        }

        return true;
    }

    private boolean getUserMemberships(Element subscriptions) throws NodeStoreException, InterruptedException {
        // let's get all subscriptions.
        ResultSet<NodeMembership> cur;
        cur = channelManager.getUserMemberships(actorJid);

        if ((null != requestIq.getElement().element(XMLConstants.PUBSUB_ELEM).element("set")) && (!cur.isEmpty())
                && (!channelManager.isLocalJID(actorJid))) {
            makeRemoteRequest(actorJid.getDomain());
            return false;
        }
        for (NodeMembership ns : cur) {
            Element subscription = subscriptions.addElement(XMLConstants.SUBSCRIPTION_ELEM);
            subscription.addAttribute(XMLConstants.NODE_ATTR, ns.getNodeId())
                    .addAttribute(XMLConstants.SUBSCRIPTION_ELEM, ns.getSubscription().toString())
                    .addAttribute(XMLConstants.JID_ATTR, ns.getUser().toBareJID());
            if (null != ns.getInvitedBy()) {
                subscription.addAttribute(XMLConstants.INVITED_BY_ELEM, ns.getInvitedBy().toBareJID());
            }
        }
        return true;
    }

    private void makeRemoteRequest(String to) throws InterruptedException {
        IQ forwarder = requestIq.createCopy();
        forwarder.setTo(to);
        if (null == forwarder.getElement().element(XMLConstants.PUBSUB_ELEM).element("actor")) {
            Element actor = forwarder.getElement().element(XMLConstants.PUBSUB_ELEM).addElement("actor", Buddycloud.NS);
            actor.addText(requestIq.getFrom().toBareJID());
        }
        outQueue.put(forwarder);
    }

    @Override
    public boolean accept(Element elm) {
        return XMLConstants.SUBSCRIPTIONS_ELEM.equals(elm.getName());
    }
}
