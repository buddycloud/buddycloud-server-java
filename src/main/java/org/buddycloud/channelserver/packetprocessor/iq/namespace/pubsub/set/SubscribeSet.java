package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.event.Event;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscribeSet extends PubSubElementProcessorAbstract {

  private static final String FIREHOSE = "/firehose";
  private static final Logger LOGGER = Logger.getLogger(SubscribeSet.class);

  public static final String MISSING_NODE_ID = "nodeid-required";
  public static final String INVALID_JID = "invalid-jid";

  public static final String INVALID_NODE_FORMAT = "invalid-node-format";

  private final BlockingQueue<Packet> outQueue;
  private final ChannelManager channelManager;

  public SubscribeSet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
    this.outQueue = outQueue;
    this.channelManager = channelManager;

    acceptedElementName = XMLConstants.SUBSCRIBE_ELEM;
  }

  @Override
  public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {

    node =
        reqIQ.getChildElement().element(acceptedElementName).attributeValue(XMLConstants.NODE_ATTR);
    request = reqIQ;

    if ((node == null) || "".equals(node)) {
      missingNodeName();
      return;
    }

    JID subscribingJid = request.getFrom();

    boolean isLocalSubscriber = false;

    if (actorJID != null) {
      subscribingJid = actorJID;
    } else {
      isLocalSubscriber = Configuration.getInstance().isLocalJID(subscribingJid);
      // Check that user is registered.
      if (!isLocalSubscriber) {
        failAuthRequired();
        return;
      }
    }

    Map<String, String> nodeConf = null;

    if (node.equals(FIREHOSE)) {
      if (!channelManager.nodeExists(FIREHOSE)) {
        channelManager.addRemoteNode(FIREHOSE);
      }
      nodeConf = new HashMap<String, String>();
      nodeConf.put(Conf.DEFAULT_AFFILIATION, "member");
      nodeConf.put(Conf.ACCESS_MODEL, "open");
    } else {
      if (!handleNodeSubscription(elm, actorJID, subscribingJid)) {
        return;
      }
      nodeConf = channelManager.getNodeConf(node);
    }

    // Subscribe to a node.
    try {

      NodeMembership membership = channelManager.getNodeMembership(node, subscribingJid);

      if (Affiliations.outcast.toString().equals(membership.getAffiliation().toString())) {
        /*
         * 6.1.3.8 Blocked <iq type='error' from='pubsub.shakespeare.lit'
         * to='francisco@denmark.lit/barracks' id='sub1'> <error type='auth'> <forbidden
         * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> </error> </iq>
         */
        IQ reply = IQ.createResultIQ(request);
        reply.setType(Type.error);
        PacketError pe =
            new PacketError(org.xmpp.packet.PacketError.Condition.forbidden,
                org.xmpp.packet.PacketError.Type.auth);
        reply.setError(pe);
        outQueue.put(reply);
        return;
      }

      Affiliations defaultAffiliation = Affiliations.member;
      Subscriptions defaultSubscription = Subscriptions.none;

      if (!membership.getSubscription().equals(Subscriptions.invited)
          && !membership.getSubscription().in(Subscriptions.none)
          && !membership.getAffiliation().in(Affiliations.none)) {
        LOGGER.debug("User already has a '" + membership.getSubscription().toString()
            + "' subscription");
        defaultAffiliation = membership.getAffiliation();
        defaultSubscription = membership.getSubscription();
      } else {
        try {
          String nodeDefaultAffiliation = nodeConf.get(Conf.DEFAULT_AFFILIATION);
          LOGGER.debug("Node default affiliation: '" + nodeDefaultAffiliation + "'");
          if (!Affiliations.none.equals(Affiliations.createFromString(nodeDefaultAffiliation))) {
            defaultAffiliation = Affiliations.createFromString(nodeDefaultAffiliation);
          }
        } catch (NullPointerException e) {
          LOGGER.error("Could not create affiliation.", e);
          defaultAffiliation = Affiliations.member;
        }
        defaultSubscription = Subscriptions.subscribed;
        String accessModel = nodeConf.get(Conf.ACCESS_MODEL);
        if ((null == accessModel)
            || (true == accessModel.equals(AccessModels.authorize.toString()))
            || (true == accessModel.equals(AccessModels.whitelist.toString()))) {
          defaultSubscription = Subscriptions.pending;
        } else if ((true == accessModel.equals(AccessModels.local.toString()) && (false == Configuration
            .getInstance().isLocalJID(subscribingJid)))) {
          defaultSubscription = Subscriptions.pending;
        }

        NodeSubscription newSubscription =
            new NodeSubscriptionImpl(node, subscribingJid, request.getFrom(), defaultSubscription,
                null);
        channelManager.addUserSubscription(newSubscription);

        if (!membership.getAffiliation().in(Affiliations.none)) {
          defaultAffiliation = membership.getAffiliation();
        }

        if (channelManager.isEphemeralNode(node) && !defaultAffiliation.equals(Affiliations.owner)) {
          defaultAffiliation = Affiliations.moderator;
        }
        channelManager.setUserAffiliation(node, subscribingJid, defaultAffiliation);
      }

      IQ reply = IQ.createResultIQ(request);
      Element pubsub = reply.setChildElement(XMLConstants.PUBSUB_ELEM, JabberPubsub.NAMESPACE_URI);
      pubsub.addElement("subscription").addAttribute("node", node)
          .addAttribute("jid", subscribingJid.toBareJID())
          .addAttribute("subscription", defaultSubscription.toString());
      pubsub.addElement("affiliation").addAttribute("node", node)
          .addAttribute("jid", subscribingJid.toBareJID())
          .addAttribute("affiliation", defaultAffiliation.toString());

      outQueue.put(reply);

      notifySubscribers(defaultSubscription, defaultAffiliation, subscribingJid);

    } catch (NodeStoreException e) {
      IQ reply = IQ.createResultIQ(request);
      reply.setType(Type.error);
      PacketError pe =
          new PacketError(PacketError.Condition.internal_server_error, PacketError.Type.wait);
      reply.setError(pe);
      outQueue.put(reply);
    }
  }

  private boolean handleNodeSubscription(Element elm, JID actorJID, JID subscribingJid)
      throws NodeStoreException, InterruptedException {
    boolean isLocalNode = false;
    try {
      isLocalNode = Configuration.getInstance().isLocalNode(node);
    } catch (IllegalArgumentException e) {
      LOGGER.debug(e);
      createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request,
          INVALID_NODE_FORMAT, Buddycloud.NS_ERROR);
      outQueue.put(response);
      return false;
    }
    if (!isLocalNode && !node.equals("/firehose")) {
      makeRemoteRequest();
      return false;
    }

    // 6.1.3.1 JIDs Do Not Match

    // Covers where we have juliet@shakespeare.lit/the-balcony
    JID jid = new JID(request.getChildElement().element("subscribe").attributeValue("jid"));
    if (!subscribingJid.toBareJID().equals(jid.toBareJID())) {

      /*
       * // 6.1.3.1 JIDs Do Not Match <iq type='error' from='pubsub.shakespeare.lit'
       * to='francisco@denmark.lit/barracks' id='sub1'> <error type='modify'> <bad-request
       * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> <invalid-jid
       * xmlns='http://jabber.org/protocol/pubsub#errors'/> </error> </iq>
       */

      IQ reply = IQ.createResultIQ(request);
      reply.setType(Type.error);

      Element badRequest =
          new DOMElement("bad-request", new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
      Element nodeIdRequired =
          new DOMElement(INVALID_JID, new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
      Element error = new DOMElement("error");
      error.addAttribute("type", PacketError.Type.modify.toXMPP());
      error.add(badRequest);
      error.add(nodeIdRequired);
      reply.setChildElement(error);
      outQueue.put(reply);
      return false;
    }

    if (!channelManager.nodeExists(node)) {
      IQ reply = IQ.createResultIQ(request);
      reply.setType(Type.error);
      PacketError pe =
          new PacketError(PacketError.Condition.item_not_found, PacketError.Type.cancel);
      reply.setError(pe);
      outQueue.put(reply);
      return false;
    }
    return true;
  }

  private void notifySubscribers(Subscriptions subscriptionStatus, Affiliations affiliationType,
      JID subscribingJid) throws NodeStoreException, InterruptedException {

    ResultSet<NodeSubscription> subscribers = channelManager.getNodeSubscriptionListeners(node);

    // Get all the affiliated users (so we can work out moderators)
    // isOwnerModerator == false as we don't let outcast's know
    ResultSet<NodeMembership> nodeMemberships = channelManager.getNodeMemberships(node);
    HashSet<JID> moderatorOwners = new HashSet<JID>();

    for (NodeMembership nodeMembership : nodeMemberships) {
      if (nodeMembership.getAffiliation().in(Affiliations.owner, Affiliations.moderator)) {
        moderatorOwners.add(nodeMembership.getUser());
      }
    }

    Document document = getDocumentHelper();
    Element message = document.addElement("message");
    message.addAttribute("remote-server-discover", "false");
    Element event = message.addElement("event", Event.NAMESPACE);
    Element subscription = event.addElement("subscription");
    message.addAttribute("from", request.getTo().toString());
    message.addAttribute("type", "headline");
    subscription.addAttribute("subscription", subscriptionStatus.toString());
    subscription.addAttribute("jid", subscribingJid.toBareJID());
    subscription.addAttribute("node", node);

    Element affiliations = event.addElement("affiliations");
    Element affiliation = affiliations.addElement("affiliation");
    affiliation.addAttribute("node", node);
    affiliation.addAttribute("jid", subscribingJid.toBareJID());
    affiliation.addAttribute("affiliation", affiliationType.toString());

    Message rootElement = new Message(message);

    for (NodeSubscription subscriber : subscribers) {

      Message notification = rootElement.createCopy();
      notification.setTo(subscriber.getListener());
      outQueue.put(notification);
      if (moderatorOwners.contains(subscriber.getUser())
          && subscriptionStatus.equals(Subscriptions.pending)) {
        outQueue.put(getPendingSubscriptionNotification(subscriber.getListener().toBareJID(),
            subscribingJid.toBareJID()));
      }
    }
    Collection<JID> admins = getAdminUsers();
    for (JID admin : admins) {
      Message notification = rootElement.createCopy();
      notification.setTo(admin);
      outQueue.put(notification);
    }
  }

  private Message getPendingSubscriptionNotification(String receiver, String subscriber) {

    Document document = getDocumentHelper();
    Element message = document.addElement("message");
    message.addAttribute("from", request.getTo().toString());
    message.addAttribute("type", "headline");
    message.addAttribute("to", receiver);
    DataForm dataForm = new DataForm(DataForm.Type.form);
    dataForm.addInstruction("Allow " + subscriber + " to subscribe to node " + node + "?");
    dataForm.setTitle("Confirm channel subscription");
    FormField formType = dataForm.addField();
    formType.addValue(JabberPubsub.NS_AUTHORIZATION);
    formType.setType(FormField.Type.hidden);
    formType.setVariable("FORM_TYPE");
    FormField subscribingNode = dataForm.addField();
    subscribingNode.setType(FormField.Type.text_single);
    subscribingNode.setVariable(JabberPubsub.VAR_NODE);
    subscribingNode.setLabel("Node");
    subscribingNode.addValue(node);
    FormField jid = dataForm.addField();
    jid.setType(FormField.Type.jid_single);
    jid.addValue(subscriber);
    jid.setLabel("Subscriber Address");
    jid.setVariable(JabberPubsub.VAR_SUBSCRIBER_JID);
    FormField allow = dataForm.addField();
    allow.setLabel("Allow " + subscriber + " to subscribe to posts of " + node + "?");
    allow.setVariable(JabberPubsub.VAR_ALLOW);
    allow.addValue("false");
    allow.setType(FormField.Type.boolean_type);
    message.add(dataForm.getElement());
    return new Message(message);
  }

  private void failAuthRequired() throws InterruptedException {
    // If the packet did not have actor, and the sender is not a local user
    // subscription is not allowed.

    /*
     * <iq type='error' from='pubsub.shakespeare.lit' to='hamlet@denmark.lit/elsinore' id='create1'>
     * <error type='auth'> <registration-required xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
     * </error> </iq>
     */

    IQ reply = IQ.createResultIQ(request);
    reply.setType(Type.error);
    PacketError pe =
        new PacketError(org.xmpp.packet.PacketError.Condition.registration_required,
            org.xmpp.packet.PacketError.Type.auth);
    reply.setError(pe);
    outQueue.put(reply);
  }

  private void missingNodeName() throws InterruptedException {
    /*
     * 7.2.3.3 NodeID Required
     * 
     * <iq type='error' from='pubsub.shakespeare.lit' to='hamlet@denmark.lit/elsinore'
     * id='retract1'> <error type='modify'> <bad-request
     * xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> <nodeid-required
     * xmlns='http://jabber.org/protocol/pubsub#errors'/> </error> </iq>
     */
    IQ reply = IQ.createResultIQ(request);
    reply.setType(Type.error);

    Element badRequest =
        new DOMElement(XMLConstants.BAD_REQUEST_ELEM, new org.dom4j.Namespace("",
            JabberPubsub.NS_XMPP_STANZAS));

    Element nodeIdRequired =
        new DOMElement(MISSING_NODE_ID, new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));

    Element error = new DOMElement(XMLConstants.ERROR_ELEM);
    error.addAttribute(XMLConstants.TYPE_ATTR, "modify");
    error.add(badRequest);
    error.add(nodeIdRequired);

    reply.setChildElement(error);

    outQueue.put(reply);
  }
}
