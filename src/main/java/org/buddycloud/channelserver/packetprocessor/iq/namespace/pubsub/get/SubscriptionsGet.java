package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
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
    
    public void setChannelManager(ChannelManager dataStore) {
        channelManager = dataStore;
    }
    
    public SubscriptionsGet(BlockingQueue<Packet> outQueue,
            ChannelManager channelManager) {
        this.outQueue = outQueue;
        this.channelManager = channelManager;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
            throws Exception {
        result = IQ.createResultIQ(reqIQ);
        actorJid = actorJID;
        requestIq = reqIQ;
        
        Element pubsub = result.setChildElement(PubSubGet.ELEMENT_NAME,
                JabberPubsub.NAMESPACE_URI);
        Element subscriptions = pubsub.addElement("subscriptions");

        node = reqIQ.getChildElement().element("subscriptions").attributeValue("node");

        if (null == actorJid) {
            actorJid = reqIQ.getFrom();
        }
        
        boolean isProcessedLocally = true;

        if (node == null) {
            isProcessedLocally = getUserMemberships(subscriptions);
        } else {
            if (false == Configuration.getInstance().isLocalNode(node)) {
                result.getElement().addAttribute("remote-server-discover", "false");
            }
            isProcessedLocally = getNodeMemberships(subscriptions);
        }
        if (false == isProcessedLocally) {
            return;
        }
        
        outQueue.put(result);
    }

    private boolean getNodeMemberships(Element subscriptions)
            throws NodeStoreException, InterruptedException {
        if (false == Configuration.getInstance().isLocalNode(node) 
            && (false == channelManager.isCachedNode(node))
        ) {
            makeRemoteRequest(new JID(node.split("/")[2]).getDomain());
            return false;
        }
        ResultSet<NodeMembership> cur;
        cur = channelManager.getNodeMemberships(node);

        subscriptions.addAttribute("node", node);

        if ((null != requestIq.getElement().element("pubsub").element("set"))
                && (0 == cur.size())
                && (false == Configuration.getInstance().isLocalNode(node))) {
            makeRemoteRequest(new JID(node.split("/")[2]).getDomain());
            return false;
        }
        boolean isOwnerModerator = isOwnerModerator();
        for (NodeMembership ns : cur) {
            if (false == actorJid.toBareJID().equals(ns.getUser())) {
                if ((false == isOwnerModerator) && ns.getAffiliation().in(Affiliations.outcast, Affiliations.none)) {
                    continue;
                }
                if ((false == isOwnerModerator) && !ns.getSubscription().equals(Subscriptions.subscribed)) {
                    continue;
                }
            }
            Element subscription = subscriptions
                    .addElement("subscription");
            subscription
                    .addAttribute("node", ns.getNodeId())
                    .addAttribute("subscription",
                            ns.getSubscription().toString())
                    .addAttribute("jid", ns.getUser().toBareJID());
            if (null != ns.getInvitedBy()) {
                subscription.addAttribute("invited-by", ns.getInvitedBy().toBareJID());
            }
        }
        return true;
    }

    private boolean isOwnerModerator() throws NodeStoreException {
        return channelManager.getNodeMembership(node, actorJid).getAffiliation().canAuthorize();
    }

    private boolean getUserMemberships(Element subscriptions)
            throws NodeStoreException, InterruptedException {
        // let's get all subscriptions.
        ResultSet<NodeMembership> cur;
        cur = channelManager.getUserMemberships(actorJid);

        if ((null != requestIq.getElement().element("pubsub").element("set"))
                && (0 == cur.size())
                && (false == Configuration.getInstance().isLocalJID(actorJid))) {
            makeRemoteRequest(actorJid.getDomain());
            return false;
        }
        for (NodeMembership ns : cur) {
            Element subscription = subscriptions
                    .addElement("subscription");
            subscription
                    .addAttribute("node", ns.getNodeId())
                    .addAttribute("subscription",
                            ns.getSubscription().toString())
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
        if (null == forwarder.getElement().element("pubsub").element("actor")) {
            Element actor = forwarder.getElement().element("pubsub")
                .addElement("actor", Buddycloud.NS);
            actor.addText(requestIq.getFrom().toBareJID());
        }
        outQueue.put(forwarder);
    }
    
    @Override
    public boolean accept(Element elm) {
        return elm.getName().equals("subscriptions");
    }
}
