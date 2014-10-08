package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
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

public class AffiliationsGet implements PubSubElementProcessor {

    private final BlockingQueue<Packet> outQueue;
    private final ChannelManager channelManager;

    private IQ requestIq;
    private String node;
    private JID actorJid;
    private IQ result;
    private String firstItem;
    private static final Logger LOGGER = Logger.getLogger(AffiliationsGet.class);

    public AffiliationsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        this.outQueue = outQueue;
        this.channelManager = channelManager;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        result = IQ.createResultIQ(reqIQ);
        requestIq = reqIQ;
        actorJid = actorJID;
        node = elm.attributeValue("node");
        if (false == channelManager.isLocalJID(requestIq.getFrom())) {
            result.getElement().addAttribute("remote-server-discover", "false");
        }
        String namespace = JabberPubsub.NS_PUBSUB_OWNER;
        if (node == null) {
            namespace = JabberPubsub.NAMESPACE_URI;
        }

        Element pubsub = result.setChildElement(PubSubGet.ELEMENT_NAME, namespace);
        Element affiliations = pubsub.addElement("affiliations");

        if (actorJid == null) {
            actorJid = requestIq.getFrom();
        }

        boolean isProcessedLocally = true;
        if (node == null) {
            isProcessedLocally = getUserMemberships(affiliations);
        } else {
            isProcessedLocally = getNodeAffiliations(affiliations);
        }
        if (false == isProcessedLocally) {
            return;
        }

        outQueue.put(result);
    }

    private boolean getNodeAffiliations(Element affiliations) throws NodeStoreException, InterruptedException {
        if (false == channelManager.isLocalNode(node) && (false == channelManager.isCachedNode(node))) {
            makeRemoteRequest(node.split("/")[2]);
            return false;
        }
        ResultSet<NodeMembership> nodeMemberships;
        nodeMemberships = channelManager.getNodeMemberships(node);

        if ((0 == nodeMemberships.size()) && (false == channelManager.isLocalNode(node))) {
            makeRemoteRequest(node.split("/")[2]);
            return false;
        }

        boolean isOwnerModerator = isOwnerModerator();

        for (NodeMembership nodeMembership : nodeMemberships) {

            if (false == actorJid.toBareJID().equals(nodeMembership.getUser().toBareJID())) {
                if ((false == isOwnerModerator) && nodeMembership.getAffiliation().in(Affiliations.outcast, Affiliations.none)) {
                    continue;
                }
                if ((false == isOwnerModerator) && !nodeMembership.getSubscription().equals(Subscriptions.subscribed)) {
                    continue;
                }
            }
            LOGGER.trace("Adding affiliation for " + nodeMembership.getUser() + " affiliation " + nodeMembership.getAffiliation());

            if (null == firstItem) {
                firstItem = nodeMembership.getUser().toString();
            }
            nodeMembership.getUser().toString();

            affiliations.addElement("affiliation").addAttribute("node", nodeMembership.getNodeId())
                    .addAttribute("affiliation", nodeMembership.getAffiliation().toString()).addAttribute("jid", nodeMembership.getUser().toString());
        }
        return true;
    }

    private boolean isOwnerModerator() throws NodeStoreException {
        return channelManager.getNodeMembership(node, actorJid).getAffiliation().canAuthorize();
    }

    private boolean getUserMemberships(Element affiliations) throws NodeStoreException, InterruptedException {

        if (false == channelManager.isLocalJID(actorJid) && (false == channelManager.isCachedJID(requestIq.getFrom()))) {
            makeRemoteRequest(actorJid.getDomain());
            return false;
        }

        ResultSet<NodeMembership> memberships;
        memberships = channelManager.getUserMemberships(actorJid);
        boolean isOwnerModerator = isOwnerModerator();

        for (NodeMembership membership : memberships) {

            if (false == actorJid.toBareJID().equals(membership.getUser().toBareJID())) {
                if ((false == isOwnerModerator) && membership.getAffiliation().in(Affiliations.outcast, Affiliations.none)) {
                    continue;
                }
                if ((false == isOwnerModerator) && !membership.getSubscription().equals(Subscriptions.subscribed)) {
                    continue;
                }
            }
            LOGGER.trace("Adding affiliation for " + membership.getUser() + " affiliation " + membership.getAffiliation() + " (no node provided)");

            if (null == firstItem) {
                firstItem = membership.getNodeId();
            }
            membership.getNodeId();

            affiliations.addElement("affiliation").addAttribute("node", membership.getNodeId())
                    .addAttribute("affiliation", membership.getAffiliation().toString()).addAttribute("jid", membership.getUser().toBareJID());
        }
        return true;
    }

    private void makeRemoteRequest(String node) throws InterruptedException {
        LOGGER.info("Going federated for <affiliations />");
        requestIq.setTo(new JID(node).getDomain());
        if (null == requestIq.getElement().element("pubsub").element("actor")) {
            Element actor = requestIq.getElement().element("pubsub").addElement("actor", Buddycloud.NS);
            actor.addText(requestIq.getFrom().toBareJID());
        }
        outQueue.put(requestIq);
    }

    @Override
    public boolean accept(Element elm) {
        return elm.getName().equals("affiliations");
    }
}
