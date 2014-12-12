package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeMembershipWithConfiguration;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class SubscriptionsWithConfigurationGet extends PubSubElementProcessorAbstract {

  private static final String ALLOW_EL = "allow";
  private static final String CONFIGURATION_FILTER_EL = "configuration-filter";
  private static final String SUBSCRIPTION_FILTER_EL = "subscription-filter";
  
  private static final String VALUE_ATT = "value";
  private static final String FIELD_ATT = "field";

  public SubscriptionsWithConfigurationGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
    this.outQueue = outQueue;
    this.channelManager = channelManager;
  }

  @SuppressWarnings("unchecked")
  @Override
  public void process(Element subscriptionsEl, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
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
    Element pubsubReplyEl = result.setChildElement(
        XMLConstants.PUBSUB_ELEM, JabberPubsub.NAMESPACE_URI);
    Element subscriptionsReplyEl = pubsubReplyEl.addElement(
        XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM, Buddycloud.NS);

    Map<String, String> subscriptionsFilter = new HashMap<String, String>();
    Element subscriptionFilterEl = subscriptionsEl.element(SUBSCRIPTION_FILTER_EL);
    if (subscriptionFilterEl != null) {
      List<Element> allowSubscriptionEls = subscriptionFilterEl.elements(ALLOW_EL);
      for (Element allowSubscriptionEl : allowSubscriptionEls) {
        subscriptionsFilter.put(allowSubscriptionEl.attributeValue(FIELD_ATT), 
            allowSubscriptionEl.attributeValue(VALUE_ATT));
      }
    }
    
    List<String> configurationFilter = new LinkedList<String>();
    Element configurationFilterEl = subscriptionsEl.element(CONFIGURATION_FILTER_EL);
    if (configurationFilterEl != null) {
      List<Element> allowConfigurationEls = configurationFilterEl.elements(ALLOW_EL);
      for (Element allowConfigurationEl : allowConfigurationEls) {
        configurationFilter.add(allowConfigurationEl.attributeValue(FIELD_ATT));
      }
    }
    
    getUserMemberships(subscriptionsReplyEl, configurationFilter, subscriptionsFilter);
    outQueue.put(result);
  }

  private void getUserMemberships(Element subscriptionsRespEl, 
      List<String> configurationFilter, Map<String, String> subscriptionsFilter) throws NodeStoreException,
      InterruptedException {

    ResultSet<NodeMembershipWithConfiguration> cur = channelManager.getUserMembershipsWithConfiguration(actor, configurationFilter, subscriptionsFilter);

    for (NodeMembershipWithConfiguration ns : cur) {
      Element subscription = subscriptionsRespEl.addElement(XMLConstants.SUBSCRIPTION_WITH_CONFIGURATION_ELEM);
      String nodeId = ns.getMembership().getNodeId();
      subscription.addAttribute(XMLConstants.NODE_ATTR, nodeId)
          .addAttribute(XMLConstants.SUBSCRIPTION_ELEM, ns.getMembership().getSubscription().toString())
          .addAttribute(XMLConstants.JID_ATTR, ns.getMembership().getUser().toBareJID());
      addNodeConfigurationElement(nodeId, ns.getConfiguration(), subscription);
    }
  }
  
  private void addNodeConfigurationElement(String node, 
      Map<String, String> nodeConf, Element subscriptionEl) throws NodeStoreException {
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
    return XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM.equals(elm.getName());
  }

}
