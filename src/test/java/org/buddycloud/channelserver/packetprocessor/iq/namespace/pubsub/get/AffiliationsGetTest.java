package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class AffiliationsGetTest extends IQTestHandler {

  private IQ userRequest;
  private IQ nodeRequest;
  private AffiliationsGet affiliationsGet;
  private Element element;
  private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

  private String node = "/user/pamela@denmark.lit/posts";
  private JID jid = new JID("juliet@shakespeare.lit");
  private JID invitedBy = new JID("romeo@shakespeare.lit");

  private ChannelManager channelManager;

  @Before
  public void setUp() throws Exception {

    queue = new LinkedBlockingQueue<Packet>();
    affiliationsGet = new AffiliationsGet(queue, channelManager);
    element = new BaseElement("affiliations");

    channelManager = Mockito.mock(ChannelManager.class);
    Configuration.getInstance().putProperty(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER,
        Boolean.TRUE.toString());

    NodeMembership nodeMembership =
        new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.member, null);
    Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class)))
        .thenReturn(nodeMembership);

    affiliationsGet.setChannelManager(channelManager);

    Configuration.getInstance().putProperty(Configuration.CONFIGURATION_SERVER_DOMAIN,
        "shakespeare.lit");
    Configuration.getInstance().putProperty(Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN,
        "topics.shakespeare.lit");
    Configuration.getInstance().remove(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);

    userRequest = readStanzaAsIq("/iq/pubsub/affiliations/request-no-node.stanza");
    nodeRequest = readStanzaAsIq("/iq/pubsub/affiliations/request-with-node.stanza");

    Mockito.when(channelManager.isCachedJID(Mockito.any(JID.class))).thenReturn(true);
  }

  @Test
  public void passingAffiliationsAsElementNameReturnsTrue() {
    Assert.assertTrue(affiliationsGet.accept(element));
  }

  @Test
  public void passingNotAffiliationsAsElementNameReturnsFalse() {
    Element element = new BaseElement("not-affiliations");
    Assert.assertFalse(affiliationsGet.accept(element));
  }

  @Test
  public void remoteNodeForwardsStanza() throws Exception {

    Mockito.when(channelManager.isCachedNode(Mockito.anyString())).thenReturn(false);
    Configuration.getInstance().putProperty(Configuration.CONFIGURATION_SERVER_DOMAIN,
        "denmark.lit");

    affiliationsGet.process(element, jid, nodeRequest, null);

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.get, response.getType());
    Assert.assertEquals(new JID("shakespeare.lit"), response.getTo());
    Assert.assertEquals(nodeRequest.getID(), response.getID());
  }

  @Test
  public void remoteJIDForwardsStanza() throws Exception {

    Configuration.getInstance().putProperty(Configuration.CONFIGURATION_SERVER_DOMAIN,
        "denmark.lit");

    Mockito.when(channelManager.isCachedJID(Mockito.any(JID.class))).thenReturn(false);
    affiliationsGet.process(element, jid, userRequest, null);

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.get, response.getType());
    Assert.assertEquals(new JID("shakespeare.lit"), response.getTo());
    Assert.assertEquals(userRequest.getID(), response.getID());
  }

  /*
   * NODE AFFILIATIONS
   */


  @Test
  public void ifNodeDataIsNotCachedThenWeGetDataFromRemote() throws Exception {

    Mockito.when(channelManager.isCachedNode(Mockito.anyString())).thenReturn(true);
    Configuration.getInstance().putProperty(Configuration.CONFIGURATION_SERVER_DOMAIN,
        "denmark.lit");

    Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(
        new ResultSetImpl<NodeMembership>(new ArrayList<NodeMembership>()));

    affiliationsGet.process(element, jid, nodeRequest, null);

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.get, response.getType());
    Assert.assertEquals(new JID("shakespeare.lit"), response.getTo());
    Assert.assertEquals(nodeRequest.getID(), response.getID());
  }

  @Test
  public void returnsAListOfNodeAffiliations() throws Exception {

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.publisher,
        invitedBy));

    Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(
        new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, jid, nodeRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.result, response.getType());
    Assert.assertEquals(new JID("francisco@shakespeare.lit/barracks"), response.getTo());
    Assert.assertEquals(nodeRequest.getID(), response.getID());

    Element pubsub = response.getElement().element("pubsub");
    Assert.assertNotNull(pubsub);
    Assert.assertEquals(JabberPubsub.NAMESPACE_URI, pubsub.getNamespaceURI());
    Element affiliations = pubsub.element("affiliations");
    Assert.assertNotNull(affiliations);
    Assert.assertEquals(1, affiliations.elements("affiliation").size());

    Element affiliation = affiliations.element("affiliation");

    Assert.assertEquals(node, affiliation.attributeValue("node"));
    Assert.assertEquals(Affiliations.publisher.toString(),
        affiliation.attributeValue("affiliation"));
    Assert.assertEquals(jid.toBareJID(), affiliation.attributeValue("jid"));
  }

  @Test
  public void nodeAffiliationsDoesNotIncludeInvitedPendingNoneByDefault() throws Exception {

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, invitedBy, Subscriptions.pending,
        Affiliations.publisher, invitedBy));
    members.add(new NodeMembershipImpl(node, invitedBy, Subscriptions.none, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, invitedBy, Subscriptions.invited,
        Affiliations.publisher, invitedBy));

    Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(
        new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, jid, nodeRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Element pubsub = response.getElement().element("pubsub");
    Element affiliations = pubsub.element("affiliations");
    Assert.assertEquals(0, affiliations.elements("affiliation").size());

  }

  @Test
  public void nodeAffiliationsIncludesInvitedAndPendingIfUser() throws Exception {

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.pending, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.none, Affiliations.publisher,
        invitedBy));

    Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(
        new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, jid, nodeRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Element pubsub = response.getElement().element("pubsub");
    Element affiliations = pubsub.element("affiliations");
    Assert.assertEquals(2, affiliations.elements("affiliation").size());

    /* Includes invited by details */
    Assert.assertEquals(invitedBy.toBareJID(),
        affiliations.element("affiliation").attributeValue("invited-by"));

  }

  @Test
  public void nodeAffiliationsIncludesInvitedAndPendingIfOwner() throws Exception {

    NodeMembership nodeMembership =
        new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.owner, null);
    Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class)))
        .thenReturn(nodeMembership);

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.pending, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.none, Affiliations.publisher,
        invitedBy));

    Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(
        new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, invitedBy, nodeRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Element pubsub = response.getElement().element("pubsub");
    Element affiliations = pubsub.element("affiliations");
    Assert.assertEquals(2, affiliations.elements("affiliation").size());

    /* Includes invited by details */
    Assert.assertEquals(invitedBy.toBareJID(),
        affiliations.element("affiliation").attributeValue("invited-by"));

  }

  @Test
  public void nodeAffiliationsIncludesInvitedAndPendingIfModerator() throws Exception {

    NodeMembership nodeMembership =
        new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.moderator, null);
    Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class)))
        .thenReturn(nodeMembership);

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.pending, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.none, Affiliations.publisher,
        invitedBy));

    Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(
        new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, invitedBy, nodeRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Element pubsub = response.getElement().element("pubsub");
    Element affiliations = pubsub.element("affiliations");
    Assert.assertEquals(2, affiliations.elements("affiliation").size());

    /* Includes invited by details */
    Assert.assertEquals(invitedBy.toBareJID(),
        affiliations.element("affiliation").attributeValue("invited-by"));

  }


  /*
   * USER AFFILIATIONS
   */

  @Test
  public void returnsAListOfUserAffiliations() throws Exception {

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.publisher,
        invitedBy));

    Mockito.when(channelManager.getUserMemberships(Mockito.any(JID.class), Mockito.eq(false)))
        .thenReturn(new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, jid, userRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.result, response.getType());
    Assert.assertEquals(new JID("francisco@shakespeare.lit/barracks"), response.getTo());
    Assert.assertEquals(nodeRequest.getID(), response.getID());

    Element pubsub = response.getElement().element("pubsub");
    Assert.assertNotNull(pubsub);
    Assert.assertEquals(JabberPubsub.NS_PUBSUB_OWNER, pubsub.getNamespaceURI());
    Element affiliations = pubsub.element("affiliations");
    Assert.assertNotNull(affiliations);
    Assert.assertEquals(1, affiliations.elements("affiliation").size());

    Element affiliation = affiliations.element("affiliation");

    Assert.assertEquals(node, affiliation.attributeValue("node"));
    Assert.assertEquals(Affiliations.publisher.toString(),
        affiliation.attributeValue("affiliation"));
    Assert.assertEquals(jid.toBareJID(), affiliation.attributeValue("jid"));
  }

  @Test
  public void userAffiliationsDoesNotIncludeInvitedNoneByDefault() throws Exception {

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, invitedBy, Subscriptions.none, Affiliations.publisher,
        invitedBy));

    Mockito.when(channelManager.getUserMemberships(Mockito.any(JID.class), Mockito.eq(false)))
        .thenReturn(new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, jid, userRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Element pubsub = response.getElement().element("pubsub");
    Element affiliations = pubsub.element("affiliations");
    Assert.assertEquals(0, affiliations.elements("affiliation").size());

  }

  @Test
  public void addsInvitedByDetailsIfSubscriptionIsInvited() throws Exception {

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.publisher,
        invitedBy));

    Mockito.when(channelManager.getUserMemberships(Mockito.any(JID.class), Mockito.eq(false)))
        .thenReturn(new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, jid, userRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Element pubsub = response.getElement().element("pubsub");
    Element affiliations = pubsub.element("affiliations");
    Assert.assertEquals(2, affiliations.elements("affiliation").size());

    /* Includes invited by details */
    Assert.assertEquals(invitedBy.toBareJID(),
        ((Element) affiliations.elements("affiliation").get(0)).attributeValue("invited-by"));
    Assert.assertNull(((Element) affiliations.elements("affiliation").get(1))
        .attribute("invited-by"));
  }

  @Test
  public void notProvidingActorSetsFromRequestToAddress() throws Exception {

    List<NodeMembership> members = new ArrayList<NodeMembership>();
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher,
        invitedBy));
    members.add(new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.publisher,
        invitedBy));

    Mockito.when(channelManager.getUserMemberships(Mockito.any(JID.class), Mockito.eq(false)))
        .thenReturn(new ResultSetImpl<NodeMembership>(members));

    affiliationsGet.process(element, null, userRequest, null);

    Assert.assertEquals(1, queue.size());

    IQ response = (IQ) queue.poll();

    Element pubsub = response.getElement().element("pubsub");
    Element affiliations = pubsub.element("affiliations");
    Assert.assertEquals(2, affiliations.elements("affiliation").size());

    /* Includes invited by details */
    Assert.assertEquals(invitedBy.toBareJID(),
        ((Element) affiliations.elements("affiliation").get(0)).attributeValue("invited-by"));
    Assert.assertNull(((Element) affiliations.elements("affiliation").get(1))
        .attribute("invited-by"));
  }

  @Test
  public void canRequestAffiliationsForEphemeralOnlyNodes() throws Exception {
    IQ request = userRequest.createCopy();
    Element affiliations =
        request.getElement().element(XMLConstants.PUBSUB_ELEM)
            .element(XMLConstants.AFFILIATIONS_ELEM);
    affiliations.addNamespace("bc", Buddycloud.NS);
    affiliations.addAttribute("bc:ephemeral", "true");

    try {
      affiliationsGet.process(element, jid, request, null);
    } catch (NullPointerException e) {

    }

    Mockito.verify(channelManager, Mockito.times(1)).getUserMemberships(Mockito.any(JID.class),
        Mockito.eq(true));
  }

  @Test
  public void notProvidingEphemeralAttributeResultsInNotEphemeralNodeGathering() throws Exception {

    try {
      affiliationsGet.process(element, jid, userRequest, null);
    } catch (NullPointerException e) {

    }

    Mockito.verify(channelManager, Mockito.times(1)).getUserMemberships(Mockito.any(JID.class),
        Mockito.eq(false));
  }

  @Test
  public void providingAnIncorrectValueForEphemeralAttributeResultsInNotEphemeralGathering()
      throws Exception {
    IQ request = userRequest.createCopy();
    Element affiliations =
        request.getElement().element(XMLConstants.PUBSUB_ELEM)
            .element(XMLConstants.AFFILIATIONS_ELEM);
    affiliations.addNamespace("bc", Buddycloud.NS);
    affiliations.addAttribute("bc:ephemeral", "sure");

    try {
      affiliationsGet.process(element, jid, request, null);
    } catch (NullPointerException e) {

    }

    Mockito.verify(channelManager, Mockito.times(1)).getUserMemberships(Mockito.any(JID.class),
        Mockito.eq(false));
  }

}
