package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class NodeDeleteTest extends IQTestHandler {

    private ChannelManager channelManager;
    private LinkedBlockingQueue<Packet> queue;
    private NodeDelete nodeDelete;
    private BaseElement element;
    private JID jid = new JID("juliet@shakespeare.lit");

    @Before
    public void setUp() throws Exception {
        this.channelManager = Mockito.mock(ChannelManager.class);
        this.queue = new LinkedBlockingQueue<Packet>();
        this.nodeDelete = new NodeDelete(queue, channelManager);
        this.nodeDelete.setServerDomain("shakespeare.lit");
        this.element = new BaseElement("delete");
    }

    @After
    public void tearDown() {
        Mockito.reset(channelManager);
    }

    @Test
    public void testPassingDeleteAsElementName() {
        Assert.assertTrue(nodeDelete.accept(element));
    }

    @Test
    public void testPassingNotDeleteAsElementName() {
        Element element = new BaseElement("not-delete");
        Assert.assertFalse(nodeDelete.accept(element));
    }

    @Test
    public void testStanzaWithNoNode() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-no-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");
        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals("nodeid-required", error.getApplicationConditionName());
    }

    @Test
    public void testStanzaWithEmptyNode() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-empty-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");
        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals("nodeid-required", error.getApplicationConditionName());
    }

    @Test
    public void testNonLocalNode() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-with-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(false);

        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        Assert.assertNull(response.getError());
        Element actorEl = response.getElement().element("pubsub").element("actor");
        Assert.assertEquals(jid.toBareJID(), actorEl.getText());
    }

    @Test
    public void testInexistentNode() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-with-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
        Mockito.when(channelManager.nodeExists(node)).thenReturn(false);

        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.cancel, error.getType());
        Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
    }

    @Test
    public void testNotRegisteredActor() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-with-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
        Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
        nodeDelete.setServerDomain("fake.domain");

        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
    }

    @Test
    public void testNotAffiliatedActor() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-with-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
        Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
        Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
                new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.none, null));

        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.not_authorized, error.getCondition());
    }

    @Test
    public void testActorIsNotOwner() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-with-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
        Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

        Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
                new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.moderator, null));

        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.not_authorized, error.getCondition());
    }

    @Test
    public void testBadFormattedNode() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-bad-formatted-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
        Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
    }

    @Test
    public void testNodeFromUnknownDomain() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-unknown-domain-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
        Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

        nodeDelete.process(deleteEl, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals(PacketError.Condition.not_acceptable, error.getCondition());
    }

    @Test
    public void testSuccessfulDeleteNoSubscribers() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-with-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
        Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

        Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
                new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.owner, null));

        nodeDelete.process(deleteEl, jid, request, null);
        IQ response = (IQ) queue.poll();

        Assert.assertNull(response.getError());
        Assert.assertEquals(request.getID(), response.getID());
        Assert.assertEquals(Type.result, response.getType());

        int adminCount = Configuration.getInstance().getAdminUsers().size();
        Assert.assertEquals(adminCount, queue.size());
    }

    @Test
    public void testSuccessfulDeleteWithSubscribers() throws Exception {
        IQ request = readStanzaAsIq("/iq/pubsub/delete/request-with-node.stanza");
        Element deleteEl = request.getChildElement().element("delete");

        String node = deleteEl.attributeValue("node");
        Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
        Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

        Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
                new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.owner, null));

        JID subscriberJid = new JID("subscriber@shakespeare.lit");
        NodeSubscriptionImpl subscription = new NodeSubscriptionImpl(node, subscriberJid, subscriberJid, Subscriptions.subscribed, null);
        List<NodeSubscription> subscriptions = new LinkedList<NodeSubscription>();
        subscriptions.add(subscription);

        Mockito.when(channelManager.getNodeSubscriptionListeners(node)).thenReturn(new ResultSetImpl<NodeSubscription>(subscriptions));

        nodeDelete.process(deleteEl, jid, request, null);
        IQ response = (IQ) queue.poll();

        Assert.assertNull(response.getError());
        Assert.assertEquals(request.getID(), response.getID());
        Assert.assertEquals(Type.result, response.getType());

        Message subscriberNotification = (Message) queue.poll();
        Assert.assertNotNull(subscriberNotification);
        Assert.assertEquals(subscriberJid, subscriberNotification.getTo());

        Element eventEl = subscriberNotification.getElement().element("event");
        Assert.assertNotNull(eventEl);
        Assert.assertEquals(node, eventEl.element("delete").attributeValue("node"));
    }
}
