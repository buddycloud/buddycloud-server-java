package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscriptionsGet implements PubSubElementProcessor {

    public static final String ELEMENT_NAME = "subscription";

    public static final String CONTAINER_ELEMENT_NAME = "subscriptions";

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
        Element pubsub = result.setChildElement(PubSubGet.ELEMENT_NAME, JabberPubsub.NAMESPACE_URI);
        Element subscriptions = pubsub.addElement(CONTAINER_ELEMENT_NAME);

        node = reqIQ.getChildElement().element(CONTAINER_ELEMENT_NAME).attributeValue("node");

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

            subscriptions.addAttribute("node", node);

            for (NodeMembership ns : cur) {
                if (actorJid.toBareJID().equals(ns.getUser().toBareJID())) {
                    Element subscription = subscriptions.addElement(ELEMENT_NAME);
                    subscription.addAttribute("node", ns.getNodeId()).addAttribute(ELEMENT_NAME, ns.getSubscription().toString())
                            .addAttribute("jid", ns.getUser().toBareJID());
                    if (null != ns.getInvitedBy()) {
                        subscription.addAttribute("invited-by", ns.getInvitedBy().toBareJID());
                    }
                }

            }
        } else {

            if (!channelManager.isCachedNode(node) || (null != requestIq.getElement().element(PubSubGet.ELEMENT_NAME).element("set")) && !cur.isEmpty()) {
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

        if ((null != requestIq.getElement().element(PubSubGet.ELEMENT_NAME).element("set")) && (!cur.isEmpty()) && (!channelManager.isLocalJID(actorJid))) {
            makeRemoteRequest(actorJid.getDomain());
            return false;
        }
        for (NodeMembership ns : cur) {
            Element subscription = subscriptions.addElement(ELEMENT_NAME);
            subscription.addAttribute("node", ns.getNodeId()).addAttribute(ELEMENT_NAME, ns.getSubscription().toString())
                    .addAttribute("jid", ns.getUser().toBareJID());
            if (null != ns.getInvitedBy()) {
                subscription.addAttribute("invited-by", ns.getInvitedBy().toBareJID());
            }
        }
        return true;
    }

    private void makeRemoteRequest(String to) throws InterruptedException {
        IQ forwarder = requestIq.createCopy();
        forwarder.setTo(to);
        if (null == forwarder.getElement().element(PubSubGet.ELEMENT_NAME).element("actor")) {
            Element actor = forwarder.getElement().element(PubSubGet.ELEMENT_NAME).addElement("actor", Buddycloud.NS);
            actor.addText(requestIq.getFrom().toBareJID());
        }
        outQueue.put(forwarder);
    }

    @Override
    public boolean accept(Element elm) {
        return CONTAINER_ELEMENT_NAME.equals(elm.getName());
    }
}
