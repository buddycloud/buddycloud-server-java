package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SubscriptionsGetTest extends IQTestHandler {

    private IQ userRequest;
    private IQ nodeRequest;
    private SubscriptionsGet subscriptionsGet;
    private Element element;
    private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

    private String node = "/user/pamela@denmark.lit/posts";
    private JID jid = new JID("juliet@shakespeare.lit");
    private JID invitedBy = new JID("romeo@shakespeare.lit");

    private ChannelManager channelManager;

    @Before
    public void setUp() throws Exception {

        queue = new LinkedBlockingQueue<Packet>();
        subscriptionsGet = new SubscriptionsGet(queue, channelManager);
        element = new BaseElement("subscriptions");

        channelManager = Mockito.mock(ChannelManager.class);
        Configuration.getInstance().putProperty(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());

        NodeMembership nodeMembership = new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.member, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class))).thenReturn(nodeMembership);
        subscriptionsGet.setChannelManager(channelManager);

        userRequest = readStanzaAsIq("/iq/pubsub/subscriptions/request.stanza");
        nodeRequest = readStanzaAsIq("/iq/pubsub/subscriptions/requestExistingNode.stanza");

    }

    @Test
    public void testPassingSubscriptionsAsElementNameReturnsTrue() {
        Assert.assertTrue(subscriptionsGet.accept(element));
    }

    @Test
    public void testPassingNotSubscriptionsAsElementNameReturnsFalse() {
        Element element = new BaseElement("not-subscriptions");
        Assert.assertFalse(subscriptionsGet.accept(element));
    }

    @Test
    public void addsInvitedByToUserSubscriptionsList() throws Exception {

        ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
        members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher, invitedBy));

        Mockito.when(channelManager.getUserMemberships(Mockito.any(JID.class), Mockito.eq(false))).thenReturn(new ResultSetImpl<NodeMembership>(members));

        subscriptionsGet.process(element, jid, userRequest, null);

        Assert.assertEquals(1, queue.size());

        IQ response = (IQ) queue.poll();
        Assert.assertEquals(IQ.Type.result, response.getType());
        Assert.assertEquals(userRequest.getTo(), response.getFrom());
        Assert.assertEquals(userRequest.getFrom(), response.getTo());
        Assert.assertEquals(userRequest.getID(), response.getID());
        Assert.assertEquals(1, response.getChildElement().element("subscriptions").elements("subscription").size());

    }

    // ------------- node subscripton tests

    @Test
    public void remoteNodeForwardsStanza() throws Exception {

        Configuration.getInstance().putProperty(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.FALSE.toString());

        Mockito.when(channelManager.isCachedNode(Mockito.anyString())).thenReturn(false);

        subscriptionsGet.process(element, jid, nodeRequest, null);

        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.get, response.getType());
        Assert.assertEquals(new JID("denmark.lit"), response.getTo());
        Assert.assertEquals(userRequest.getID(), response.getID());
    }


    @Test
    public void doesntAddInvitedByToNodeSubscriptionsListIfNotUserOrOwnerOrModerator() throws Exception {

        List<NodeMembership> members = new ArrayList<NodeMembership>();
        members.add(new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.publisher, invitedBy));

        Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(new ResultSetImpl<NodeMembership>(members));

        // Run the processing as a JID other than the one used in the above 'add'
        subscriptionsGet.process(element, new JID("shylock@shakespeare.lit"), nodeRequest, null);

        Assert.assertEquals(1, queue.size());

        IQ response = (IQ) queue.poll();
        Assert.assertEquals(IQ.Type.result, response.getType());
        Assert.assertEquals(userRequest.getTo(), response.getFrom());
        Assert.assertEquals(userRequest.getFrom(), response.getTo());
        Assert.assertEquals(userRequest.getID(), response.getID());
        Element subscription = (Element) response.getChildElement().element("subscriptions").elements("subscription").get(0);
        Assert.assertNull(subscription.attributeValue("invited-by"));

    }

    @Test
    public void addsInvitedByToNodeSubscriptionsListIfOwner() throws Exception {

        NodeMembership nodeMembership = new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.owner, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class))).thenReturn(nodeMembership);

        ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
        members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher, invitedBy));

        Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(new ResultSetImpl<NodeMembership>(members));

        subscriptionsGet.process(element, jid, nodeRequest, null);

        Assert.assertEquals(1, queue.size());

        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.result, response.getType());
        Assert.assertEquals(userRequest.getTo(), response.getFrom());
        Assert.assertEquals(userRequest.getFrom(), response.getTo());
        Assert.assertEquals(userRequest.getID(), response.getID());
        Assert.assertEquals(1, response.getChildElement().element("subscriptions").elements("subscription").size());
        Element subscription = (Element) response.getChildElement().element("subscriptions").elements("subscription").get(0);
        Assert.assertEquals(node, subscription.attributeValue("node"));
        Assert.assertEquals(invitedBy.toBareJID(), subscription.attributeValue("invited-by"));
        Assert.assertEquals(jid.toBareJID(), subscription.attributeValue("jid"));
        Assert.assertEquals(Subscriptions.invited.toString(), subscription.attributeValue("subscription"));
    }

    @Test
    public void addsInvitedByToNodeSubscriptionsListIfModerator() throws Exception {

        NodeMembership nodeMembership = new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.moderator, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class))).thenReturn(nodeMembership);

        ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
        members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher, invitedBy));

        Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(new ResultSetImpl<NodeMembership>(members));

        subscriptionsGet.process(element, jid, nodeRequest, null);

        Assert.assertEquals(1, queue.size());

        IQ response = (IQ) queue.poll();
        Assert.assertEquals(IQ.Type.result, response.getType());
        Assert.assertEquals(userRequest.getTo(), response.getFrom());
        Assert.assertEquals(userRequest.getFrom(), response.getTo());
        Assert.assertEquals(userRequest.getID(), response.getID());
        Assert.assertEquals(1, response.getChildElement().element("subscriptions").elements("subscription").size());

    }

    @Test
    public void addsInvitedByToUserSubscriptionsListIfUser() throws Exception {

        NodeMembership nodeMembership = new NodeMembershipImpl(node, nodeRequest.getFrom(), Subscriptions.subscribed, Affiliations.moderator, null);
        Mockito.when(channelManager.getNodeMembership(Mockito.anyString(), Mockito.any(JID.class))).thenReturn(nodeMembership);

        ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
        members.add(new NodeMembershipImpl(node, jid, Subscriptions.invited, Affiliations.publisher, invitedBy));

        Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(new ResultSetImpl<NodeMembership>(members));

        subscriptionsGet.process(element, jid, nodeRequest, null);

        Assert.assertEquals(1, queue.size());

        IQ response = (IQ) queue.poll();
        Assert.assertEquals(IQ.Type.result, response.getType());
        Assert.assertEquals(userRequest.getTo(), response.getFrom());
        Assert.assertEquals(userRequest.getFrom(), response.getTo());
        Assert.assertEquals(userRequest.getID(), response.getID());
        Assert.assertEquals(1, response.getChildElement().element("subscriptions").elements("subscription").size());

    }
    
    
    @Test
    public void canRequestUserSubscriptionsForEphemeralOnlyNodes() throws Exception {
      IQ request = userRequest.createCopy();
      Element subscriptions =
          request.getElement().element(XMLConstants.PUBSUB_ELEM)
              .element(XMLConstants.SUBSCRIPTIONS_ELEM);
      subscriptions.addNamespace("bc", Buddycloud.NS);
      subscriptions.addAttribute(new QName(
          XMLConstants.EPHEMERAL, Namespace.get(Buddycloud.NS)), "true");     

      try {
        subscriptionsGet.process(element, jid, request, null);
      } catch (NullPointerException e) {
        
      }
      Mockito.verify(channelManager, Mockito.times(1)).getUserMemberships(Mockito.any(JID.class), Mockito.eq(true));
    }
    
    @Test
    public void notProvidingEphemeralAttributeForUserSubscriptionsResultsInNotEphemeralNodeGathering() throws Exception {
      try {
        subscriptionsGet.process(element, jid, userRequest, null);
      } catch (NullPointerException e) {
        
      }
      Mockito.verify(channelManager, Mockito.times(1)).getUserMemberships(Mockito.any(JID.class), Mockito.eq(false));
    }
    
    @Test
    public void providingAnIncorrectValueForEphemeralAttributeResultsInNotEphemeralGatheringOfUserSubscriptions() throws Exception {

      IQ request = userRequest.createCopy();
      Element subscriptions =
          request.getElement().element(XMLConstants.PUBSUB_ELEM)
              .element(XMLConstants.SUBSCRIPTIONS_ELEM);
      subscriptions.addNamespace("bc", Buddycloud.NS);
      subscriptions.addAttribute(new QName(
          XMLConstants.EPHEMERAL, Namespace.get(Buddycloud.NS)), "sure");
      
      try {
        subscriptionsGet.process(element, jid, request, null);
      } catch (NullPointerException e) {
        
      }
      Mockito.verify(channelManager, Mockito.times(1)).getUserMemberships(Mockito.any(JID.class), Mockito.eq(false));
    }

}
