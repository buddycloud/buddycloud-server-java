package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.XMLConstants;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class AffiliationsGet extends PubSubElementProcessorAbstract {

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
        node = elm.attributeValue(XMLConstants.NODE_ATTR);
        if (!channelManager.isLocalJID(requestIq.getFrom())) {
            result.getElement().addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR, Boolean.FALSE.toString());
        }
        String namespace = JabberPubsub.NS_PUBSUB_OWNER;
        if (node == null) {
            namespace = JabberPubsub.NAMESPACE_URI;
        }

        Element pubsub = result.setChildElement(XMLConstants.PUBSUB_ELEM, namespace);
        Element affiliations = pubsub.addElement(XMLConstants.AFFILIATION_ELEM);

        if (actorJid == null) {
            actorJid = requestIq.getFrom();
        }

        boolean isProcessedLocally = true;
        if (node == null) {
            isProcessedLocally = getUserMemberships(affiliations);
        } else {
            isProcessedLocally = getNodeAffiliations(affiliations);
        }
        if (!isProcessedLocally) {
            return;
        }

        outQueue.put(result);
    }

    private boolean getNodeAffiliations(Element affiliations) throws NodeStoreException, InterruptedException {
        if (!channelManager.isLocalNode(node) && (!channelManager.isCachedNode(node))) {
            makeRemoteRequest(node.split("/")[2]);
            return false;
        }
        ResultSet<NodeMembership> nodeMemberships;
        nodeMemberships = channelManager.getNodeMemberships(node);

        if ((!nodeMemberships.isEmpty()) && (!channelManager.isLocalNode(node))) {
            makeRemoteRequest(node.split("/")[2]);
            return false;
        }

        for (NodeMembership nodeMembership : nodeMemberships) {

            if (actorJid.toBareJID().equals(nodeMembership.getUser().toBareJID())) {
                if (null == firstItem) {
                    firstItem = nodeMembership.getUser().toString();
                }

                affiliations.addElement(XMLConstants.AFFILIATION_ELEM).addAttribute(XMLConstants.NODE_ATTR, nodeMembership.getNodeId())
                        .addAttribute(XMLConstants.AFFILIATION_ELEM, nodeMembership.getAffiliation().toString())
                        .addAttribute(XMLConstants.JID_ATTR, nodeMembership.getUser().toString());
            }
        }

        return true;
    }

    private boolean isOwnerModerator() throws NodeStoreException {
        return channelManager.getNodeMembership(node, actorJid).getAffiliation().canAuthorize();
    }

    private boolean getUserMemberships(Element affiliations) throws NodeStoreException, InterruptedException {

        if (!channelManager.isLocalJID(actorJid) && (!channelManager.isCachedJID(requestIq.getFrom()))) {
            makeRemoteRequest(actorJid.getDomain());
            return false;
        }

        ResultSet<NodeMembership> memberships = channelManager.getUserMemberships(actorJid);
        for (NodeMembership membership : memberships) {

            if (actorJid.toBareJID().equals(membership.getUser().toBareJID())) {
                LOGGER.trace("Adding affiliation for " + membership.getUser() + " affiliation " + membership.getAffiliation() + " (no node provided)");

                if (null == firstItem) {
                    firstItem = membership.getNodeId();
                }

                affiliations.addElement(XMLConstants.AFFILIATION_ELEM).addAttribute(XMLConstants.NODE_ATTR, membership.getNodeId())
                        .addAttribute(XMLConstants.AFFILIATION_ELEM, membership.getAffiliation().toString())
                        .addAttribute(XMLConstants.JID_ATTR, membership.getUser().toBareJID());
            }

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
        return XMLConstants.AFFILIATION_ELEM.equals(elm.getName());
    }
}
