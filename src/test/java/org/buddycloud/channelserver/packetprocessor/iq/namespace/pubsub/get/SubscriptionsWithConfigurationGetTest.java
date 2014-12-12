package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembershipWithConfiguration;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipWithConfigurationImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SubscriptionsWithConfigurationGetTest extends IQTestHandler {

  private LinkedBlockingQueue<Packet> queue;
  private ChannelManager channelManager;
  private SubscriptionsWithConfigurationGet subscriptionsWithMetadataGet;
  private BaseElement element;
  
  private JID jid = new JID("juliet@shakespeare.lit");
  private String node1 = "/user/pamela@denmark.lit/posts";
  private String node2 = "/user/john@denmark.lit/posts";

  @Before
  public void setUp() throws Exception {
    this.queue = new LinkedBlockingQueue<Packet>();
    this.channelManager = Mockito.mock(ChannelManager.class);
    this.subscriptionsWithMetadataGet = new SubscriptionsWithConfigurationGet(queue, channelManager);
    this.element = new BaseElement(XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM);
    
    Configuration.getInstance().putProperty(
        Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());
  }

  @Test
  public void testPassingSubscriptionsWithMetadataAsElementNameReturnsTrue() {
    Assert.assertTrue(subscriptionsWithMetadataGet.accept(element));
  }
  
  @Test
  public void testPassingNotSubscriptionsWithMetadataAsElementNameReturnsFalse() {
    Element element = new BaseElement("not-subscriptions-with-metadata");
    Assert.assertFalse(subscriptionsWithMetadataGet.accept(element));
  }
  
  @Test
  public void testRemoteForward() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request.stanza");
    
    Configuration.getInstance().putProperty(
        Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.FALSE.toString());
    
    Element el = reqIQ.getChildElement().element(XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM);
    subscriptionsWithMetadataGet.process(el, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    IQ response = (IQ) queue.poll();
    
    Assert.assertNotNull(response.getChildElement().element("actor"));
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void testGettingNodesEmptyResponse() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request.stanza");
    
    Mockito.when(channelManager.getUserMembershipsWithConfiguration(
        Mockito.any(JID.class), Mockito.anyList(), Mockito.anyMap())).thenReturn(
            new ResultSetImpl<NodeMembershipWithConfiguration>(new LinkedList<NodeMembershipWithConfiguration>()));
    
    Element el = reqIQ.getChildElement().element(XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM);
    subscriptionsWithMetadataGet.process(el, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    IQ response = (IQ) queue.poll();
    Assert.assertEquals(0, response.getChildElement()
        .element("subscriptions-with-configuration")
        .elements().size());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void testGettingSingleNode() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request.stanza");
    
    Map<String, String> conf = new HashMap<String, String>();
    conf.put("key1", "value1");
    conf.put("key2", "value2");
    
    ArrayList<NodeMembershipWithConfiguration> subscriptions = new ArrayList<NodeMembershipWithConfiguration>();
    subscriptions.add(new NodeMembershipWithConfigurationImpl(
        new NodeMembershipImpl(node1, jid, Subscriptions.subscribed, Affiliations.publisher, null), 
        conf));
    
    Mockito.when(channelManager.getUserMembershipsWithConfiguration(
        Mockito.any(JID.class), Mockito.anyList(), Mockito.anyMap())).thenReturn(
            new ResultSetImpl<NodeMembershipWithConfiguration>(subscriptions));
    
    Element el = reqIQ.getChildElement().element(XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM);
    subscriptionsWithMetadataGet.process(el, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    
    IQ response = (IQ) queue.poll();
    Element subscriptionsEl = response.getChildElement().element("subscriptions-with-configuration");
    Assert.assertEquals(1, subscriptionsEl.elements("subscription-with-configuration").size());
    
    Element subscriptionEl = subscriptionsEl.element("subscription-with-configuration");
    Assert.assertEquals(node1, subscriptionEl.attributeValue("node"));
    Assert.assertEquals(jid.toBareJID(), subscriptionEl.attributeValue("jid"));
    
    Element xEl = subscriptionEl.element("configure").element("x");
    Assert.assertNotNull(xEl);
    
    Element field1 = getField(xEl, "key1");
    Assert.assertNotNull(field1);
    Assert.assertEquals("value1", field1.elementText("value"));
    
    Element field2 = getField(xEl, "key2");
    Assert.assertNotNull(field2);
    Assert.assertEquals("value2", field2.elementText("value"));
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void testGettingMultipleNodes() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request.stanza");
    
    Map<String, String> conf = new HashMap<String, String>();
    conf.put("key1", "value1");
    conf.put("key2", "value2");
    
    ArrayList<NodeMembershipWithConfiguration> subscriptions = new ArrayList<NodeMembershipWithConfiguration>();
    subscriptions.add(new NodeMembershipWithConfigurationImpl(
        new NodeMembershipImpl(node1, jid, Subscriptions.subscribed, Affiliations.publisher, null), 
        conf));
    subscriptions.add(new NodeMembershipWithConfigurationImpl(
        new NodeMembershipImpl(node2, jid, Subscriptions.subscribed, Affiliations.publisher, null), 
        conf));
    
    Mockito.when(channelManager.getUserMembershipsWithConfiguration(
        Mockito.any(JID.class), Mockito.anyList(), Mockito.anyMap())).thenReturn(
            new ResultSetImpl<NodeMembershipWithConfiguration>(subscriptions));
    
    Element el = reqIQ.getChildElement().element(XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM);
    subscriptionsWithMetadataGet.process(el, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    
    IQ response = (IQ) queue.poll();
    Element subscriptionsEl = response.getChildElement().element("subscriptions-with-configuration");
    Assert.assertEquals(2, subscriptionsEl.elements("subscription-with-configuration").size());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void testGettingNodesWithSubscriptionFilter() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request-with-subscription-filter.stanza");
    
    Map<String, String> conf1 = new HashMap<String, String>();
    conf1.put("key1", "value1");
    conf1.put("key2", "value2");
    
    ArrayList<NodeMembershipWithConfiguration> subscriptions = new ArrayList<NodeMembershipWithConfiguration>();
    subscriptions.add(new NodeMembershipWithConfigurationImpl(
        new NodeMembershipImpl(node1, jid, Subscriptions.subscribed, Affiliations.publisher, null), 
        conf1));
    
    Map<String, String> subscriptionFilter = new HashMap<String, String>();
    subscriptionFilter.put("key1", "value1");
    
    Mockito.when(channelManager.getUserMembershipsWithConfiguration(
        Mockito.any(JID.class), Mockito.anyList(), Mockito.eq(subscriptionFilter))).thenReturn(
            new ResultSetImpl<NodeMembershipWithConfiguration>(subscriptions));
    
    Element el = reqIQ.getChildElement().element(XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM);
    subscriptionsWithMetadataGet.process(el, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    
    IQ response = (IQ) queue.poll();
    Element subscriptionsEl = response.getChildElement().element("subscriptions-with-configuration");
    Assert.assertEquals(1, subscriptionsEl.elements("subscription-with-configuration").size());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  public void testGettingNodesWithConfigurationFilter() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request-with-configuration-filter.stanza");
    
    Map<String, String> conf1 = new HashMap<String, String>();
    conf1.put("key1", "value1");
    
    ArrayList<NodeMembershipWithConfiguration> subscriptions = new ArrayList<NodeMembershipWithConfiguration>();
    subscriptions.add(new NodeMembershipWithConfigurationImpl(
        new NodeMembershipImpl(node1, jid, Subscriptions.subscribed, Affiliations.publisher, null), 
        conf1));
    
    List<String> configurationFilter = new LinkedList<String>();
    configurationFilter.add("key1");
    
    Mockito.when(channelManager.getUserMembershipsWithConfiguration(
        Mockito.any(JID.class), Mockito.eq(configurationFilter), Mockito.anyMap())).thenReturn(
            new ResultSetImpl<NodeMembershipWithConfiguration>(subscriptions));
    
    Element el = reqIQ.getChildElement().element(XMLConstants.SUBSCRIPTIONS_WITH_CONFIGURATION_ELEM);
    subscriptionsWithMetadataGet.process(el, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    
    IQ response = (IQ) queue.poll();
    Element subscriptionsEl = response.getChildElement().element("subscriptions-with-configuration");
    Assert.assertEquals(1, subscriptionsEl.elements("subscription-with-configuration").size());
  }

  @SuppressWarnings("unchecked")
  private Element getField(Element xEl, String var) {
    List<Element> fields = xEl.elements();
    for (Element field : fields) {
      if (field.attributeValue("var").equals(var)) {
        return field;
      }
    }
    return null;
  }

}
