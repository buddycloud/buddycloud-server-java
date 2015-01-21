package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.channel.node.configuration.field.Ephemeral;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SubscribeSetTest extends IQTestHandler {
  private IQ request;
  private SubscribeSet subscribe;
  private Element element;
  private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

  private String node = "/user/pamela@denmark.lit/posts";
  private JID jid = new JID("juliet@shakespeare.lit");
  private ChannelManager channelManager;

  private NodeMembership membership;

  @Before
  public void setUp() throws Exception {

    channelManager = Mockito.mock(ChannelManager.class);
    Configuration.getInstance().putProperty(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER,
        Boolean.TRUE.toString());
    Mockito.when(channelManager.nodeExists(Mockito.anyString())).thenReturn(true);

    queue = new LinkedBlockingQueue<Packet>();
    subscribe = new SubscribeSet(queue, channelManager);
    request = readStanzaAsIq("/iq/pubsub/subscribe/request.stanza");

    element = new BaseElement("subscribe");
    element.addAttribute("node", node);

    subscribe.setChannelManager(channelManager);

    membership = new NodeMembershipImpl(node, jid, Subscriptions.none, Affiliations.none, null);

    Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class)))
        .thenReturn(membership);

    ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.member,
        null));

    Mockito.doReturn(new ResultSetImpl<NodeMembership>(members)).when(channelManager)
        .getNodeMemberships(Mockito.anyString());

    ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
    subscribers.add(new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed, null));

    Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers)).when(channelManager)
        .getNodeSubscriptionListeners(Mockito.anyString());

  }

  @Test
  public void testMissingNodeAttributeReturnsError() throws Exception {

    IQ request = this.request.createCopy();
    request.getChildElement().element("subscribe").attribute("node").detach();

    subscribe.process(element, jid, request, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.error, response.getType());

    PacketError error = response.getError();
    Assert.assertNotNull(error);

    Assert.assertEquals(PacketError.Type.modify, error.getType());
    Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
    Assert.assertEquals(SubscribeSet.MISSING_NODE_ID, error.getApplicationConditionName());

  }

  @Test
  public void testTryingToSubscribeSomeoneElseReturnsError() throws Exception {

    IQ request = this.request.createCopy();

    // We are passing in an actor of `jid`
    request.getChildElement().element("subscribe").attribute("jid")
        .setValue("francisco@denmark.lit");

    subscribe.process(element, jid, request, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.error, response.getType());

    PacketError error = response.getError();
    Assert.assertNotNull(error);

    Assert.assertEquals(PacketError.Type.modify, error.getType());
    Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
    Assert.assertEquals(SubscribeSet.INVALID_JID, error.getApplicationConditionName());

  }

  @Test
  public void testUnRegisteedLocalUserReturnsError() throws Exception {

    Configuration.getInstance().remove(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);

    IQ request = this.request.createCopy();

    subscribe.process(element, null, request, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.error, response.getType());

    PacketError error = response.getError();
    Assert.assertNotNull(error);

    Assert.assertEquals(PacketError.Type.auth, error.getType());
    Assert.assertEquals(PacketError.Condition.registration_required, error.getCondition());
  }

  @Test
  public void testLocalUserGetsSubscriptionToLocalAccessModelNode() throws Exception {
    Map<String, String> configuration = new HashMap<String, String>();
    configuration.put(Conf.ACCESS_MODEL, AccessModel.local.toString());
    configuration.put(Conf.DEFAULT_AFFILIATION, Affiliations.member.toString());

    Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(configuration);

    subscribe.process(element, null, request, null);

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.result, response.getType());
    Assert.assertNull(response.getError());

    Assert.assertEquals(
        Subscriptions.subscribed,
        Subscriptions.valueOf(response.getChildElement().element("subscription")
            .attributeValue("subscription")));
  }

  @Test
  public void testRemoteUserGetsPendingSubscriptionToLocalAccessModelNode() throws Exception {

    Configuration.getInstance().remove(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
    Configuration.getInstance().putProperty(Configuration.CONFIGURATION_SERVER_DOMAIN,
        "denmark.lit");

    Map<String, String> configuration = new HashMap<String, String>();
    configuration.put(Conf.ACCESS_MODEL, AccessModel.local.toString());
    configuration.put(Conf.DEFAULT_AFFILIATION, Affiliations.member.toString());

    Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(configuration);

    request.setFrom("francisco@barracks.lit");
    request.getElement().element("pubsub").element("subscribe")
        .addAttribute("jid", request.getFrom().toBareJID());
    subscribe.process(element, request.getFrom(), request, null);

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.result, response.getType());
    Assert.assertNull(response.getError());

    Assert.assertEquals(Subscriptions.pending.toString(),
        response.getChildElement().element("subscription").attributeValue("subscription"));

  }

  @Test
  public void testNoDefaultAffiliationConfigurationResultsInMemberAffiliation() throws Exception {

    Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(
        new HashMap<String, String>());

    subscribe.process(element, new JID("francisco@denmark.lit"), request, null);

    Mockito.verify(channelManager).setUserAffiliation(Mockito.anyString(), Mockito.any(JID.class),
        Mockito.eq(Affiliations.member));

    IQ response = (IQ) queue.poll();
    Assert.assertEquals(IQ.Type.result, response.getType());

  }

  @Test
  public void testDefaultAffiliationConfigurationResultsInCorrectAffiliation() throws Exception {

    Map<String, String> configuration = new HashMap<String, String>();
    configuration.put(Conf.ACCESS_MODEL, AccessModel.open.toString());
    configuration.put(Conf.DEFAULT_AFFILIATION, Affiliations.publisher.toString());
    Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(configuration);

    subscribe.process(element, new JID("francisco@denmark.lit"), request, null);

    Mockito.verify(channelManager).setUserAffiliation(Mockito.anyString(), Mockito.any(JID.class),
        Mockito.eq(Affiliations.publisher));

    IQ response = (IQ) queue.poll();
    Assert.assertEquals(IQ.Type.result, response.getType());

  }

  @Test
  public void ifAlreadySubscribedThenSubscriptionDetailsAreReturned() throws Exception {
    membership =
        new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.owner, null);

    Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class)))
        .thenReturn(membership);

    subscribe.process(element, new JID("francisco@denmark.lit"), request, null);

    IQ response = (IQ) queue.poll();
    Assert.assertEquals(IQ.Type.result, response.getType());
    Assert.assertEquals(
        Subscriptions.subscribed,
        Subscriptions.valueOf(response.getChildElement().element("subscription")
            .attributeValue("subscription")));
  }

  @Test
  public void canSubscribeIfInvited() throws Exception {
    Map<String, String> configuration = new HashMap<String, String>();
    configuration.put(Conf.ACCESS_MODEL, AccessModel.local.toString());
    configuration.put(Conf.DEFAULT_AFFILIATION, Affiliations.member.toString());

    Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(configuration);

    membership = new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.none, null);

    Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class)))
        .thenReturn(membership);

    subscribe.process(element, new JID("francisco@denmark.lit"), request, null);
    IQ response = (IQ) queue.poll();
    Assert.assertEquals(IQ.Type.result, response.getType());
    Assert.assertEquals(
        Subscriptions.subscribed,
        Subscriptions.valueOf(response.getChildElement().element("subscription")
            .attributeValue("subscription")));
  }

  @Test
  public void illegalNodeFormatReturnsAppropriateError() throws Exception {

    request.getElement().element("pubsub").element("subscribe")
        .addAttribute("node", "illegalformat");
    subscribe.process(element, new JID("francisco@denmark.lit"), request, null);
    IQ response = (IQ) queue.poll();
    Assert.assertEquals(IQ.Type.error, response.getType());
    PacketError error = response.getError();
    Assert.assertEquals(PacketError.Type.modify, error.getType());
    Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
    Assert.assertEquals(SubscribeSet.INVALID_NODE_FORMAT, error.getApplicationConditionName());
    Assert.assertEquals(Buddycloud.NS_ERROR, error.getApplicationConditionNamespaceURI());

  }

  @Test
  public void ifEphemerialNodeUserSubscribesAsModerator() throws Exception {

    Map<String, String> configuration = new HashMap<String, String>();
    configuration.put(Conf.ACCESS_MODEL, AccessModel.open.toString());
    configuration.put(Ephemeral.FIELD_NAME, "true");
    configuration.put(Conf.DEFAULT_AFFILIATION, Affiliations.publisher.toString());
    Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(configuration);
    Mockito.when(channelManager.isEphemeralNode(Mockito.anyString())).thenReturn(true);

    subscribe.process(element, new JID("francisco@denmark.lit"), request, null);

    Mockito.verify(channelManager).setUserAffiliation(Mockito.anyString(), Mockito.any(JID.class),
        Mockito.eq(Affiliations.moderator));

    IQ response = (IQ) queue.poll();
    Assert.assertEquals(IQ.Type.result, response.getType());
  }

}
