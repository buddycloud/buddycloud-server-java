package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.field.Ephemeral;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.NodeThreadsGet;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;
import org.xmpp.resultsetmanagement.ResultSetImpl;
import org.xmpp.util.XMPPConstants;

import sun.tools.tree.AddExpression;

public class NodeDelete extends PubSubElementProcessorAbstract {

  private static final String NODE_REG_EX = "^/user/[^@]+@[^/]+/[^/]+$";
  private ResultSet<NodeSubscription> subscriptions;

  private static final Logger LOGGER = Logger.getLogger(NodeThreadsGet.class);

  public NodeDelete(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
    setChannelManager(channelManager);
    setOutQueue(outQueue);

    acceptedElementName = "delete";
  }

  public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {

    this.element = elm;
    this.response = IQ.createResultIQ(reqIQ);
    this.request = reqIQ;
    this.actor = actorJID;
    this.node = element.attributeValue(XMLConstants.NODE_ATTR);

    if (actorJID == null) {
      actor = request.getFrom();
    }
    if (!nodePresent() || !nodeValid()) {
      outQueue.put(response);
      return;
    }
    if (!Configuration.getInstance().isLocalNode(node)) {
      makeRemoteRequest();
      return;
    }
    if (!checkNodeExists() || !actorIsRegistered() || !nodeHandledByThisServer()
        || isEphemeralNode() || !actorAllowedToDelete()) {
      outQueue.put(response);
      return;
    }
    getNodeListeners();
    deleteNode();
    sendNotifications();
  }

  private boolean isEphemeralNode() throws NodeStoreException {
    if (!channelManager.isEphemeralNode(node)) {
      return false;
    }
    createExtendedErrorReply(PacketError.Type.cancel, PacketError.Condition.not_allowed, XMLConstants.CAN_NOT_DELETE_EPHEMERAL_NODE, Buddycloud.NS_ERROR);
    return true;
  }

  private void getNodeListeners() {
    try {
      subscriptions = channelManager.getNodeSubscriptionListeners(node);
    } catch (NodeStoreException e) {
      subscriptions = new ResultSetImpl<NodeSubscription>(new ArrayList<NodeSubscription>());
    }
  }

  private void sendNotifications() throws NodeStoreException {
    try {
      Message notification = createNotificationMessage();
      if (subscriptions != null) {
        for (NodeSubscription subscription : subscriptions) {
          notification.setTo(subscription.getListener().toString());
          outQueue.put(notification.createCopy());
        }
      }
      Collection<JID> admins = getAdminUsers();
      for (JID admin : admins) {
        notification.setTo(admin);
        outQueue.put(notification.createCopy());
      }
    } catch (Exception e) {
      LOGGER.error(e);
    }
  }

  private Message createNotificationMessage() {
    Message notification = new Message();
    notification.setType(Message.Type.headline);
    notification.getElement().addAttribute("remote-server-discover", "false");
    Element eventEl = notification.addChildElement("event", JabberPubsub.NS_PUBSUB_EVENT);
    Element deleteEl = eventEl.addElement("delete");
    deleteEl.addAttribute("node", node);
    return notification;
  }

  private void deleteNode() throws InterruptedException {
    try {
      channelManager.deleteNode(node);
    } catch (NodeStoreException e) {
      LOGGER.error(e);
      setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
      outQueue.put(response);
      return;
    }
    response.setType(IQ.Type.result);
    outQueue.put(response);
  }

  private boolean actorAllowedToDelete() throws NodeStoreException {
    boolean isOwner =
        channelManager.getNodeMembership(node, actor).getAffiliation().equals(Affiliations.owner);

    if (isOwner) {
      return true;
    }
    setErrorCondition(PacketError.Type.auth, PacketError.Condition.not_authorized);
    return false;
  }

  private boolean nodeHandledByThisServer() {
    if (!node.contains("@" + getServerDomain()) && !node.contains("@" + getTopicsDomain())) {
      setErrorCondition(PacketError.Type.modify, PacketError.Condition.not_acceptable);
      return false;
    }
    return true;
  }

  private boolean nodeValid() {
    if (!node.matches(NODE_REG_EX)) {
      setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
      return false;
    }
    return true;
  }
}
