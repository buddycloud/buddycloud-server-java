package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class AffiliationsGet extends PubSubElementProcessorAbstract {

  private static final Logger LOGGER = Logger.getLogger(AffiliationsGet.class);

  public AffiliationsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
    this.outQueue = outQueue;
    this.channelManager = channelManager;
  }

  @Override
  public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
    response = IQ.createResultIQ(reqIQ);
    request = reqIQ;
    actor = actorJID;

    node = request.getChildElement().element("affiliations").attributeValue(XMLConstants.NODE_ATTR);
    if (!Configuration.getInstance().isLocalJID(request.getFrom())) {
      response.getElement().addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR,
          Boolean.FALSE.toString());

    }
    String namespace = JabberPubsub.NAMESPACE_URI;
    if (node == null) {
      namespace = JabberPubsub.NS_PUBSUB_OWNER;
    }


    Element pubsub = response.setChildElement(XMLConstants.PUBSUB_ELEM, namespace);
    Element affiliations = pubsub.addElement(XMLConstants.AFFILIATIONS_ELEM);

    if (actor == null) {
      actor = request.getFrom();
    }

    boolean isProcessedLocally =
        (node == null) ? getUserMemberships(affiliations) : getNodeAffiliations(affiliations);
    if (!isProcessedLocally) {
      return;
    }

    outQueue.put(response);
  }

  private boolean getNodeAffiliations(Element affiliations) throws NodeStoreException,
      InterruptedException {

    if (!Configuration.getInstance().isLocalNode(node) && (!channelManager.isCachedNode(node))) {

      makeRemoteRequest(node.split("/")[2]);
      return false;
    }
    ResultSet<NodeMembership> nodeMemberships;
    nodeMemberships = channelManager.getNodeMemberships(node);

    if ((nodeMemberships.isEmpty()) && (!Configuration.getInstance().isLocalNode(node))) {
      makeRemoteRequest(node.split("/")[2]);
      return false;
    }


    for (NodeMembership nodeMembership : nodeMemberships) {
      if (!nodeMembership.getSubscription().equals(Subscriptions.subscribed)
          && !actor.toBareJID().equals(nodeMembership.getUser().toBareJID()) && !isOwnerModerator()) {
        continue;
      }
      if (nodeMembership.getSubscription().equals(Subscriptions.none)) {
        continue;
      }
      Element affiliation = affiliations.addElement(XMLConstants.AFFILIATION_ELEM);
      affiliation.addAttribute(XMLConstants.NODE_ATTR, nodeMembership.getNodeId());
      affiliation.addAttribute(XMLConstants.AFFILIATION_ELEM, nodeMembership.getAffiliation()
          .toString());
      affiliation.addAttribute(XMLConstants.JID_ATTR, nodeMembership.getUser().toString());
      if ((actor.toBareJID().equals(nodeMembership.getUser().toBareJID()) || isOwnerModerator())
          && (null != nodeMembership.getInvitedBy())) {
        affiliation.addAttribute(XMLConstants.INVITED_BY_ATTR, nodeMembership.getInvitedBy()
            .toBareJID());
      }
    }

    return true;
  }

  private boolean isOwnerModerator() throws NodeStoreException {
    return channelManager.getNodeMembership(node, actor).getAffiliation().canAuthorize();
  }

  private boolean getUserMemberships(Element affiliations) throws NodeStoreException,
      InterruptedException {

    if (!channelManager.isCachedJID(request.getFrom())
        && !Configuration.getInstance().isLocalJID(actor)) {
      makeRemoteRequest(actor.getDomain());
      return false;
    }

    ResultSet<NodeMembership> memberships = channelManager.getUserMemberships(actor);

    for (NodeMembership membership : memberships) {

      if (membership.getSubscription().equals(Subscriptions.none)) {
        continue;
      }
      Element affiliation = affiliations.addElement(XMLConstants.AFFILIATION_ELEM);
      affiliation.addAttribute(XMLConstants.NODE_ATTR, membership.getNodeId());
      affiliation.addAttribute(XMLConstants.AFFILIATION_ELEM, membership.getAffiliation()
          .toString());
      affiliation.addAttribute(XMLConstants.JID_ATTR, membership.getUser().toBareJID());

      if (membership.getSubscription().equals(Subscriptions.invited)
          && (null != membership.getInvitedBy())) {
        affiliation.addAttribute(XMLConstants.INVITED_BY_ATTR, membership.getInvitedBy()
            .toBareJID());
      }

    }
    return true;
  }

  private void makeRemoteRequest(String node) throws InterruptedException {
    LOGGER.info("Going federated for <affiliations />");
    request.setTo(new JID(node).getDomain());
    if (null == request.getElement().element("pubsub").element("actor")) {
      Element actor = request.getElement().element("pubsub").addElement("actor", Buddycloud.NS);
      actor.addText(request.getFrom().toBareJID());
    }
    outQueue.put(request);
  }

  @Override
  public boolean accept(Element elm) {
    return XMLConstants.AFFILIATIONS_ELEM.equals(elm.getName());
  }
}
