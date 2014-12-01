package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscriptionsWithMetadataGet extends PubSubElementProcessorAbstract {

  public SubscriptionsWithMetadataGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
    this.outQueue = outQueue;
    this.channelManager = channelManager;
  }

  @Override
  public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
    this.actor = actorJID;
    this.request = reqIQ;

    if (null == actor) {
      actor = reqIQ.getFrom();
    }
    
    if (!Configuration.getInstance().isLocalJID(actor)) {
      makeRemoteRequest(actor.getDomain());
      return;
    }
    
    IQ result = IQ.createResultIQ(reqIQ);
    Element pubsubEl = result.setChildElement(
        XMLConstants.PUBSUB_ELEM, JabberPubsub.NAMESPACE_URI);
    Element subscriptionsEl = pubsubEl.addElement(
        XMLConstants.SUBSCRIPTIONS_WITH_METADATA_ELEM, Buddycloud.NS);

    getUserMemberships(subscriptionsEl);
    outQueue.put(result);
  }

  private void getUserMemberships(Element subscriptions) throws NodeStoreException,
      InterruptedException {

    String ephemeralValue = request.getChildElement()
        .element(XMLConstants.SUBSCRIPTIONS_WITH_METADATA_ELEM)
        .attributeValue(XMLConstants.EPHEMERAL);
    
    boolean ephemeral = false;
    if ((null != ephemeralValue) && ephemeralValue.equals("true")) {
      ephemeral = true;
    }

    ResultSet<NodeMembership> cur = channelManager.getUserMemberships(actor, ephemeral);

    for (NodeMembership ns : cur) {
      Element subscription = subscriptions.addElement(XMLConstants.SUBSCRIPTION_WITH_METADATA_ELEM);
      String nodeId = ns.getNodeId();
      subscription.addAttribute(XMLConstants.NODE_ATTR, nodeId)
          .addAttribute(XMLConstants.SUBSCRIPTION_ELEM, ns.getSubscription().toString())
          .addAttribute(XMLConstants.JID_ATTR, ns.getUser().toBareJID());
      addNodeConfigurationElement(nodeId, subscription);
    }
  }
  
  private void addNodeConfigurationElement(String node, Element subscriptionEl) throws NodeStoreException {
    Map<String, String> nodeConf = channelManager.getNodeConf(node);

    DataForm x = new DataForm(DataForm.Type.result);

    FormField formType = x.addField();
    formType.setType(FormField.Type.hidden);
    formType.setVariable("FORM_TYPE");
    formType.addValue(NodeConfigureGet.NS_CONFIGURE);

    for (String key : nodeConf.keySet()) {
      String value = nodeConf.get(key);
      x.addField(key, null, null).addValue(value);
    }

    Element configure = subscriptionEl.addElement(XMLConstants.CONFIGURE_ELEM);
    configure.addAttribute(XMLConstants.NODE_ATTR, node);
    configure.add(x.getElement());
  }
  
  private void makeRemoteRequest(String to) throws InterruptedException {
    IQ forwarder = request.createCopy();
    forwarder.setTo(to);
    if (null == forwarder.getElement().element(XMLConstants.PUBSUB_ELEM)
        .element(XMLConstants.ACTOR_ELEM)) {
      Element actor =
          forwarder.getElement().element(XMLConstants.PUBSUB_ELEM)
              .addElement(XMLConstants.ACTOR_ELEM, Buddycloud.NS);

      actor.addText(request.getFrom().toBareJID());
    }
    outQueue.put(forwarder);
  }
  
  @Override
  public boolean accept(Element elm) {
    return XMLConstants.SUBSCRIPTIONS_WITH_METADATA_ELEM.equals(elm.getName());
  }

}
