package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
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

public class AffiliationEvent extends PubSubElementProcessorAbstract {

  Element requestedAffiliationElement;
  NodeMembership usersCurrentMembership;
  private Affiliations requestedAffiliation;
  private NodeMembership membership;

  private static final Logger LOGGER = Logger.getLogger(AffiliationEvent.class);

  public static final String CAN_NOT_MODIFY_OWN_AFFILIATION = "can-not-modify-own-affiliation";

  /**
   * Constructor
   * 
   * @param outQueue Outgoing message queue
   * @param channelManager Data Access Object (DAO)
   */
  public AffiliationEvent(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
    setChannelManager(channelManager);
    setOutQueue(outQueue);

    acceptedElementName = XMLConstants.AFFILIATIONS_ELEM;
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

    if (false == Configuration.getInstance().isLocalNode(node)) {
      makeRemoteRequest();
      return;
    }

    try {

      if ((false == isValidStanza()) || (false == checkNodeExists())
          || (false == actorHasPermissionToAuthorize())
          || (false == subscriberHasCurrentAffiliation())
          || (true == userIsModifyingTheirAffiliation())
          || (false == attemptToChangeAffiliationOfNodeOwner())
          || (true == isIllegalEphemeralNodeAffiliationChange())) {
        outQueue.put(response);
        return;
      }
      saveUpdatedAffiliation();
      sendNotifications();
    } catch (NodeStoreException e) {
      LOGGER.error(e);
      setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
      outQueue.put(response);
      return;
    }
  }

  private boolean isIllegalEphemeralNodeAffiliationChange() throws NodeStoreException {
    if (false == channelManager.isEphemeralNode(node)) {
      return false;
    }
    if (requestedAffiliation.canAuthorize()) {
      return false;
    }
    setErrorCondition(PacketError.Type.cancel, PacketError.Condition.not_allowed);
    return true;
  }

  private boolean userIsModifyingTheirAffiliation() {
    if (actor.toBareJID().equals(requestedAffiliationElement.attributeValue(XMLConstants.JID_ATTR))) {
      createExtendedErrorReply(PacketError.Type.cancel, PacketError.Condition.not_allowed,
          CAN_NOT_MODIFY_OWN_AFFILIATION, Buddycloud.NS_ERROR);
      return true;
    }
    return false;
  }

  private boolean attemptToChangeAffiliationOfNodeOwner() {
    if (!usersCurrentMembership.getAffiliation().equals(Affiliations.owner)) {
      return true;
    }
    setErrorCondition(PacketError.Type.modify, PacketError.Condition.not_acceptable);
    return false;
  }

  private void sendNotifications() throws Exception {

    outQueue.put(response);

    ResultSet<NodeSubscription> subscribers = channelManager.getNodeSubscriptionListeners(node);

    Document document = getDocumentHelper();
    Element message = document.addElement(XMLConstants.MESSAGE_ELEM);
    Element pubsub = message.addElement(XMLConstants.EVENT_ELEM);
    message.addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR, Boolean.FALSE.toString());
    Element affiliations = pubsub.addElement(XMLConstants.AFFILIATIONS_ELEM);
    Element affiliation = affiliations.addElement(XMLConstants.AFFILIATION_ELEM);

    pubsub.addNamespace("", JabberPubsub.NS_PUBSUB_EVENT);
    message.addAttribute(XMLConstants.FROM_ATTR, request.getTo().toString());
    message.addAttribute(XMLConstants.TYPE_ATTR, "headline");

    affiliations.addAttribute(XMLConstants.NODE_ATTR, node);
    affiliation.addAttribute(XMLConstants.JID_ATTR,
        requestedAffiliationElement.attributeValue(XMLConstants.JID_ATTR));
    affiliation.addAttribute(XMLConstants.AFFILIATION_ELEM,
        requestedAffiliationElement.attributeValue(XMLConstants.AFFILIATION_ELEM));

    Message rootElement = new Message(message);

    for (NodeSubscription subscriber : subscribers) {
      Message notification = rootElement.createCopy();
      notification.setTo(subscriber.getListener());
      outQueue.put(notification);
    }

    Collection<JID> admins = getAdminUsers();
    for (JID admin : admins) {
      Message notification = rootElement.createCopy();
      notification.setTo(admin);
      outQueue.put(notification);
    }
  }

  private void saveUpdatedAffiliation() throws NodeStoreException {
    JID jid = new JID(requestedAffiliationElement.attributeValue(XMLConstants.JID_ATTR));
    Affiliations affiliation =
        Affiliations.valueOf(requestedAffiliationElement
            .attributeValue(XMLConstants.AFFILIATION_ELEM));

    channelManager.setUserAffiliation(node, jid, affiliation);
  }

  protected boolean isValidStanza() {
    try {
      requestedAffiliationElement =
          request.getElement().element(XMLConstants.PUBSUB_ELEM)
              .element(XMLConstants.AFFILIATIONS_ELEM).element(XMLConstants.AFFILIATION_ELEM);
      if ((null == requestedAffiliationElement)
          || (null == requestedAffiliationElement.attribute(XMLConstants.JID_ATTR))
          || (null == requestedAffiliationElement.attribute(XMLConstants.AFFILIATION_ELEM))) {
        setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
        return false;
      }
      requestedAffiliation =
          Affiliations.createFromString(requestedAffiliationElement
              .attributeValue(XMLConstants.AFFILIATION_ELEM));

    } catch (NullPointerException e) {
      LOGGER.error(e);
      setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
      return false;
    }
    requestedAffiliationElement.addAttribute(XMLConstants.AFFILIATION_ELEM,
        requestedAffiliation.toString());
    return true;
  }

  private boolean subscriberHasCurrentAffiliation() throws NodeStoreException {

    usersCurrentMembership =
        channelManager.getNodeMembership(node,
            new JID(requestedAffiliationElement.attributeValue(XMLConstants.JID_ATTR)));

    if (usersCurrentMembership.getAffiliation().equals(Affiliations.none)
        && usersCurrentMembership.getSubscription().equals(Subscriptions.none)) {
      setErrorCondition(PacketError.Type.modify, PacketError.Condition.unexpected_request);
      return false;
    }
    if (membership.getAffiliation().equals(Affiliations.moderator)
        && usersCurrentMembership.getAffiliation().canAuthorize()) {
      setErrorCondition(PacketError.Type.auth, PacketError.Condition.forbidden);
      return false;
    }
    return true;
  }

  private boolean actorHasPermissionToAuthorize() throws NodeStoreException {

    membership = channelManager.getNodeMembership(node, actor);
    if (!membership.getAffiliation().canAuthorize()) {
      setErrorCondition(PacketError.Type.auth, PacketError.Condition.not_authorized);
      return false;
    }
    if (membership.getAffiliation().equals(Affiliations.owner)) {
      return true;
    }
    if (requestedAffiliation.equals(Affiliations.moderator)
        || requestedAffiliation.equals(Affiliations.owner)) {
      setErrorCondition(PacketError.Type.auth, PacketError.Condition.forbidden);
      return false;
    }
    return true;
  }
}
