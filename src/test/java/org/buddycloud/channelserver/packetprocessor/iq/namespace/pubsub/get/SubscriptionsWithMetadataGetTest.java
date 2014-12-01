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
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SubscriptionsWithMetadataGetTest extends IQTestHandler {

  private LinkedBlockingQueue<Packet> queue;
  private ChannelManager channelManager;
  private SubscriptionsWithMetadataGet subscriptionsWithMetadataGet;
  private BaseElement element;
  
  private JID jid = new JID("juliet@shakespeare.lit");
  private String node1 = "/user/pamela@denmark.lit/posts";
  private String node2 = "/user/john@denmark.lit/posts";

  @Before
  public void setUp() throws Exception {
    this.queue = new LinkedBlockingQueue<Packet>();
    this.channelManager = Mockito.mock(ChannelManager.class);
    this.subscriptionsWithMetadataGet = new SubscriptionsWithMetadataGet(queue, channelManager);
    this.element = new BaseElement("subscriptions-with-metadata");
    
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
    
    subscriptionsWithMetadataGet.process(element, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    IQ response = (IQ) queue.poll();
    
    Assert.assertNotNull(response.getChildElement().element("actor"));
  }
  
  @Test
  public void testGettingNonEphemeralNodesEmptyResponse() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request.stanza");
    
    Mockito.when(channelManager.getUserMemberships(
        Mockito.any(JID.class), Mockito.eq(false))).thenReturn(new ResultSetImpl<NodeMembership>(new LinkedList<NodeMembership>()));
    
    subscriptionsWithMetadataGet.process(element, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    IQ response = (IQ) queue.poll();
    Assert.assertEquals(0, response.getChildElement()
        .element("subscriptions-with-metadata")
        .elements().size());
  }
  
  @Test
  public void testGettingEphemeralNodesEmptyResponse() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request-ephemeral.stanza");
    
    Mockito.when(channelManager.getUserMemberships(
        Mockito.any(JID.class), Mockito.eq(true))).thenReturn(new ResultSetImpl<NodeMembership>(new LinkedList<NodeMembership>()));
    
    subscriptionsWithMetadataGet.process(element, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    IQ response = (IQ) queue.poll();
    Assert.assertEquals(0, response.getChildElement()
        .element("subscriptions-with-metadata")
        .elements().size());
  }
  
  @Test
  public void testGettingNonEphemeralSingleNode() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request.stanza");
    
    ArrayList<NodeMembership> subscriptions = new ArrayList<NodeMembership>();
    subscriptions.add(new NodeMembershipImpl(node1, jid, Subscriptions.subscribed, Affiliations.publisher, null));
    
    Mockito.when(channelManager.getUserMemberships(
        Mockito.any(JID.class), Mockito.eq(false))).thenReturn(new ResultSetImpl<NodeMembership>(subscriptions));
    
    Map<String, String> conf = new HashMap<String, String>();
    conf.put("key1", "value1");
    conf.put("key2", "value2");
    
    Mockito.when(channelManager.getNodeConf(Mockito.eq(node1))).thenReturn(conf);
    
    subscriptionsWithMetadataGet.process(element, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    
    IQ response = (IQ) queue.poll();
    Element subscriptionsEl = response.getChildElement().element("subscriptions-with-metadata");
    Assert.assertEquals(1, subscriptionsEl.elements("subscription-with-metadata").size());
    
    Element subscriptionEl = subscriptionsEl.element("subscription-with-metadata");
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
  
  @Test
  public void testGettingNonEphemeralMultipleNodes() throws Exception {
    IQ reqIQ = readStanzaAsIq("/iq/pubsub/subscriptions-with-metadata/request.stanza");
    
    ArrayList<NodeMembership> subscriptions = new ArrayList<NodeMembership>();
    subscriptions.add(new NodeMembershipImpl(node1, jid, Subscriptions.subscribed, Affiliations.publisher, null));
    subscriptions.add(new NodeMembershipImpl(node2, jid, Subscriptions.subscribed, Affiliations.publisher, null));
    
    Mockito.when(channelManager.getUserMemberships(
        Mockito.any(JID.class), Mockito.eq(false))).thenReturn(new ResultSetImpl<NodeMembership>(subscriptions));
    
    Map<String, String> conf = new HashMap<String, String>();
    conf.put("key1", "value1");
    conf.put("key2", "value2");
    
    Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(conf);
    
    subscriptionsWithMetadataGet.process(element, jid, reqIQ, null);
    Assert.assertEquals(1, queue.size());
    
    IQ response = (IQ) queue.poll();
    Element subscriptionsEl = response.getChildElement().element("subscriptions-with-metadata");
    Assert.assertEquals(2, subscriptionsEl.elements("subscription-with-metadata").size());
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
