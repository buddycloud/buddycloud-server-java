package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class UserItemsGetTest extends IQTestHandler {

    private IQ request;
    private PubSubElementProcessorAbstract userItemsGet;
    private Element element;
    private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

    private String node = "/user/pamela@denmark.lit/posts";
    private JID jid = new JID("juliet@shakespeare.lit");
    private ChannelManager channelManager;

    private static final String TEST_NODE_1 = "node1";
    private static final String TEST_NODE_2 = "node2";

    @Before
    public void setUp() throws Exception {

        queue = new LinkedBlockingQueue<Packet>();
        channelManager = Mockito.mock(ChannelManager.class);

        userItemsGet = new UserItemsGet(queue, channelManager);

        request = readStanzaAsIq("/iq/pubsub/user-items/request.stanza");
        element = new BaseElement("user-items");
    }

    @Test
    public void testPassingUserItemsAsElementNameReturnsTrue() {
        Assert.assertTrue(userItemsGet.accept(element));
    }

    @Test
    public void testPassingNotUserItemsAsElementNameReturnsFalse() {
        Element element = new BaseElement("not-user-items");
        Assert.assertFalse(userItemsGet.accept(element));
    }

    @Test
    public void testMissingSinceAttributeReturnsErrorStanza() throws Exception {

        request.getChildElement().element("user-items").addAttribute("since", null);

        userItemsGet.process(element, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals("since-required", error.getApplicationConditionName());
    }

    @Test
    public void testInvalidSinceAttributesReturnsErrorStanza() throws Exception {

        request.getChildElement().element("user-items").addAttribute("since", "a week ago");

        userItemsGet.process(element, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals("invalid-since-value-provided", error.getApplicationConditionName());
    }

    @Test
    public void testNodeStoreExceptionGeneratesAnErrorStanza() throws Exception {

        Mockito.when(
                channelManager.getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(), Mockito.any(GlobalItemID.class),
                        Mockito.anyBoolean())).thenThrow(new NodeStoreException());

        userItemsGet.process(element, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.wait, error.getType());
        Assert.assertEquals(PacketError.Condition.internal_server_error, error.getCondition());
    }

    @Test
    public void testNoUserItemsReturnsEmptyStanza() throws Exception {

        Mockito.when(
                channelManager.getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(), Mockito.any(GlobalItemID.class),
                        Mockito.anyBoolean())).thenReturn(new ClosableIteratorImpl<NodeItem>(new ArrayList<NodeItem>().iterator()));

        userItemsGet.process(element, jid, request, null);
        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.result, response.getType());
        Element pubsub = response.getChildElement();
        Assert.assertEquals("pubsub", pubsub.getName());
        Assert.assertEquals(JabberPubsub.NAMESPACE_URI, pubsub.getNamespaceURI());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testOutgoingStanzaFormattedAsExpected() throws Exception {

        NodeItem item1 = new NodeItemImpl(TEST_NODE_1, "1", new Date(), "<entry>item1</entry>");
        NodeItem item2 = new NodeItemImpl(TEST_NODE_2, "1", new Date(), "<entry>item2</entry>");
        NodeItem item3 = new NodeItemImpl(TEST_NODE_1, "2", new Date(), "<entry>item3</entry>");
        NodeItem item4 = new NodeItemImpl(TEST_NODE_1, "3", new Date(), "<entry>item4</entry>");

        ArrayList<NodeItem> results = new ArrayList<NodeItem>();
        results.add(item1);
        results.add(item2);
        results.add(item3);
        results.add(item4);

        Mockito.when(
                channelManager.getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(), Mockito.any(GlobalItemID.class),
                        Mockito.anyBoolean())).thenReturn(new ClosableIteratorImpl<NodeItem>(results.iterator()));

        userItemsGet.process(element, jid, request, null);
        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.result, response.getType());
        Element pubsub = response.getChildElement();
        Assert.assertEquals("pubsub", pubsub.getName());
        Assert.assertEquals(JabberPubsub.NAMESPACE_URI, pubsub.getNamespaceURI());

        List<Element> items = pubsub.elements("items");
        Assert.assertEquals(3, items.size());

        Assert.assertEquals(TEST_NODE_1, items.get(0).attributeValue("node"));
        Assert.assertEquals(TEST_NODE_2, items.get(1).attributeValue("node"));
        Assert.assertEquals(TEST_NODE_1, items.get(2).attributeValue("node"));

        Assert.assertEquals(1, items.get(0).elements("item").size());
        Assert.assertEquals(2, items.get(2).elements("item").size());
    }

    @Test
    public void testUnparsableItemEntriesAreSimplyIgnored() throws Exception {

        NodeItem item1 = new NodeItemImpl(TEST_NODE_1, "1", new Date(), "<entry>item1</entry>");
        NodeItem item2 = new NodeItemImpl(TEST_NODE_1, "2", new Date(), "<entry>item2");

        ArrayList<NodeItem> results = new ArrayList<NodeItem>();
        results.add(item1);
        results.add(item2);

        Mockito.when(
                channelManager.getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(), Mockito.any(GlobalItemID.class),
                        Mockito.anyBoolean())).thenReturn(new ClosableIteratorImpl<NodeItem>(results.iterator()));

        userItemsGet.process(element, jid, request, null);
        IQ response = (IQ) queue.poll();
        Assert.assertEquals(1, response.getChildElement().element("items").elements("item").size());
    }

    @SuppressWarnings("unchecked")
    @Test
    @Ignore
    public void testCanControlGatheredEntriesUsingRsm() throws Exception {

        NodeItem item1 = new NodeItemImpl(TEST_NODE_1, "node1:1", new Date(0), "<entry>item1</entry>");
        NodeItem item2 = new NodeItemImpl(TEST_NODE_2, "node2:1", new Date(10), "<entry>item2</entry>");
        NodeItem item3 = new NodeItemImpl(TEST_NODE_1, "node1:2", new Date(20), "<entry>item3</entry>");
        NodeItem item4 = new NodeItemImpl(TEST_NODE_1, "node1:3", new Date(30), "<entry>item4</entry>");

        ArrayList<NodeItem> results = new ArrayList<NodeItem>();
        results.add(item1);
        results.add(item2);
        results.add(item3);
        results.add(item4);

        Mockito.when(
                channelManager.getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(), Mockito.any(GlobalItemID.class),
                        Mockito.anyBoolean())).thenReturn(new ClosableIteratorImpl<NodeItem>(results.iterator()));
        Mockito.when(channelManager.getCountUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyBoolean())).thenReturn(
                results.size());

        Element rsm = request.getElement().addElement("rsm");
        rsm.addNamespace("", PubSubElementProcessorAbstract.NS_RSM);
        rsm.addElement("max").addText("2");
        rsm.addElement("after").addText("node1:1");

        userItemsGet.process(element, jid, request, null);
        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.result, response.getType());
        Element pubsub = response.getChildElement();
        Assert.assertEquals("pubsub", pubsub.getName());
        Assert.assertEquals(JabberPubsub.NAMESPACE_URI, pubsub.getNamespaceURI());

        List<Element> items = pubsub.elements("items");
        Assert.assertEquals(2, items.size());

        Assert.assertEquals(TEST_NODE_2, items.get(0).attributeValue("node"));
        Assert.assertEquals(TEST_NODE_1, items.get(1).attributeValue("node"));
        Assert.assertEquals(1, items.get(0).elements("item").size());
        Assert.assertEquals(1, items.get(1).elements("item").size());

        Element rsmResult = pubsub.element("set");
        Assert.assertEquals("2", rsmResult.element("count").getText());
        Assert.assertEquals("node2:1", rsmResult.element("first").getText());
        Assert.assertEquals("node1:2", rsmResult.element("last").getText());
    }

    @SuppressWarnings("serial")
    @Test
    public void testPagingAfterItem() throws Exception {
        Element rsm = new BaseElement(new QName("set", new Namespace("", "http://jabber.org/protocol/rsm")));

        GlobalItemID itemID = new GlobalItemIDImpl(new JID("capulet.lit"), "/user/juliet@capulet.lit/posts", "item1");

        rsm.addElement("after").setText(itemID.toString());
        rsm.addElement("max").setText("5");

        element.addAttribute("node", node);

        Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);

        Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
                new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.member, null));

        ArrayList<NodeItem> results = new ArrayList<NodeItem>() {
            {
                add(new NodeItemImpl(TEST_NODE_1, "entry1", new Date(System.currentTimeMillis()), "<entry><id>entry1</id></entry>"));
                add(new NodeItemImpl(TEST_NODE_2, "entry2", new Date(System.currentTimeMillis() - 100), "<entry><id>entry2</id></entry>"));
            }
        };

        Mockito.when(
                channelManager.getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(), Mockito.any(GlobalItemID.class),
                        Mockito.anyBoolean())).thenReturn(new ClosableIteratorImpl<NodeItem>(results.iterator()));
        Mockito.when(channelManager.getCountUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyBoolean())).thenReturn(100);

        userItemsGet.process(element, jid, request, rsm);

        verify(channelManager).getUserFeedItems(eq(jid), any(Date.class), eq(5), eq(itemID), Mockito.anyBoolean());

        Packet p = queue.poll(100, TimeUnit.MILLISECONDS);

        // Check the response has a valid rsm element
        Element rsmOut = p.getElement().element("pubsub").element("set");

        assertEquals("Unexpected count returned", "100", rsmOut.element("count").getText());
        assertEquals("Unexpected first returned", TEST_NODE_1 + ",entry1", rsmOut.element("first").getText());
        assertEquals("Unexpected last returned", TEST_NODE_2 + ",entry2", rsmOut.element("last").getText());
    }

    @Test
    public void testPagingAfterItemWithInvalidAfterId() throws Exception {
        Element rsm = new BaseElement(new QName("set", new Namespace("", "http://jabber.org/protocol/rsm")));

        rsm.addElement("after").setText("this is invalid");

        element.addAttribute("node", "/user/francisco@denmark.lit/posts");

        Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);

        Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
                new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.member, null));

        Mockito.when(
                channelManager.getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(), Mockito.any(GlobalItemID.class),
                        Mockito.eq(false))).thenReturn(new ClosableIteratorImpl<NodeItem>(new ArrayList<NodeItem>().iterator()));
        Mockito.when(channelManager.getCountUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.eq(false))).thenReturn(0);

        userItemsGet.process(element, jid, request, rsm);

        Packet p = queue.poll(100, TimeUnit.MILLISECONDS);

        assertEquals("Error expected", "error", p.getElement().attributeValue("type"));
    }

    @Test
    public void whenRequestingParentOnlyCorrectFlagIsSetOnDatabaseRequest() throws Exception {
        Element rsm = new BaseElement(new QName("set", new Namespace("", "http://jabber.org/protocol/rsm")));

        element.addAttribute("node", "/user/francisco@denmark.lit/posts");

        IQ request = this.request.createCopy();
        request.getChildElement().element("user-items").addAttribute("parent-only", "true");

        Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);

        Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
                new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.member, null));

        Mockito.when(
                channelManager.getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(), Mockito.any(GlobalItemID.class),
                        Mockito.anyBoolean())).thenReturn(new ClosableIteratorImpl<NodeItem>(new ArrayList<NodeItem>().iterator()));
        Mockito.when(channelManager.getCountUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyBoolean())).thenReturn(0);

        userItemsGet.process(element, jid, request, rsm);

        Mockito.verify(channelManager, Mockito.times(1)).getUserFeedItems(Mockito.any(JID.class), Mockito.any(Date.class), Mockito.anyInt(),
                Mockito.any(GlobalItemID.class), Mockito.eq(true));
    }

    /**
     * Issue #230
     */
    @Test
    public void badRsmValueReturnsValidError() throws Exception {

        String after = "/user/demo@buddycloud.com/posts/example.com|94948";
        Element rsm = new BaseElement(new QName("set", new Namespace("", "http://jabber.org/protocol/rsm")));
        rsm.addElement("after").setText(after);

        String stanza =
                "<iq type=\"get\" to=\"channels.buddycloud.org\" " + "from=\"test.user@buddycloud.org/resource\" id=\"id:1\">"
                        + "<pubsub xmlns=\"http://jabber.org/protocol/pubsub\">" + "<user-items xmlns=\"http://buddycloud.org/v1\" parent-only=\"true\" "
                        + "since=\"2000-01-01T00:00:00.000Z\"/>" + "<set xmlns=\"http://jabber.org/protocol/rsm\">" + "<max>10</max>"
                        + "<after>/user/demo@buddycloud.com/posts/example.com|94948</after>" + "</set>" + "</pubsub>" + "</iq>";
        IQ request = this.toIq(stanza);

        userItemsGet.process(element, jid, request, rsm);
        Assert.assertEquals(1, queue.size());

        IQ response = (IQ) queue.poll();
        PacketError errorPacket = response.getError();
        Assert.assertEquals("Could not parse the 'after' id: " + after, errorPacket.getText());
        Assert.assertEquals(PacketError.Condition.bad_request, errorPacket.getCondition());
        Assert.assertEquals(PacketError.Type.modify, errorPacket.getType());


    }
}
