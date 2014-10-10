package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.special;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.RecentItemsGet;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class FirehoseGetTest extends IQTestHandler {

    private IQ request;
    private FirehoseGet recentItemsGet;
    private Element element;
    private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

    private JID jid = new JID("user1@server1");
    private ChannelManager channelManager;

    private static final String TEST_NODE_1 = "node1";
    private static final String TEST_NODE_2 = "node2";

    @Before
    public void setUp() throws Exception {

        queue = new LinkedBlockingQueue<Packet>();
        channelManager = Mockito.mock(ChannelManager.class);

        recentItemsGet = new FirehoseGet(queue, channelManager);
        new IQTestHandler();
        request = readStanzaAsIq("/iq/pubsub/items/request.stanza");
        element = new BaseElement("items");
        
        readConf();
    }

    @Test
    public void testPassingItemsAsElementNameReturnsTrue() {
        Assert.assertTrue(recentItemsGet.accept(element));
    }

    @Test
    public void testPassingNotItemsAsElementNameReturnsFalse() {
        Element element = new BaseElement("not-items");
        Assert.assertFalse(recentItemsGet.accept(element));
    }

    @Test
    public void testNodeStoreExceptionGeneratesAnErrorStanza() throws Exception {

        Mockito.when(
                channelManager.getFirehose(Mockito.anyInt(),
                        Mockito.anyString(), Mockito.anyBoolean(), 
                        Mockito.anyString())).thenThrow(
                new NodeStoreException());

        recentItemsGet.process(element, jid, request, null);
        Packet response = queue.poll();
        
        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.wait, error.getType());
        Assert.assertEquals(PacketError.Condition.internal_server_error,
                error.getCondition());
    }

    @Test
    public void testItemsReturnsEmptyStanza() throws Exception {

        Mockito.when(
                channelManager.getFirehose(Mockito.anyInt(), Mockito.anyString(), 
                        Mockito.anyBoolean(), Mockito.anyString())).thenReturn(
                new ClosableIteratorImpl<NodeItem>(new ArrayList<NodeItem>()
                        .iterator()));

        recentItemsGet.process(element, jid, request, null);
        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.result, response.getType());
        Element pubsub = response.getChildElement();
        Assert.assertEquals("pubsub", pubsub.getName());
        Assert.assertEquals(JabberPubsub.NAMESPACE_URI,
                pubsub.getNamespaceURI());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testOutgoingStanzaFormattedAsExpected() throws Exception {

        NodeItem item1 = new NodeItemImpl(TEST_NODE_1, "1", new Date(),
                "<entry>item1</entry>");
        NodeItem item2 = new NodeItemImpl(TEST_NODE_2, "1", new Date(),
                "<entry>item2</entry>");
        NodeItem item3 = new NodeItemImpl(TEST_NODE_1, "2", new Date(),
                "<entry>item3</entry>");
        NodeItem item4 = new NodeItemImpl(TEST_NODE_1, "3", new Date(),
                "<entry>item4</entry>");

        ArrayList<NodeItem> results = new ArrayList<NodeItem>();
        results.add(item1);
        results.add(item2);
        results.add(item3);
        results.add(item4);

        Mockito.when(
                channelManager.getFirehose(Mockito.anyInt(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyString())).thenReturn(
                new ClosableIteratorImpl<NodeItem>(results.iterator()));
        Mockito.when(
                channelManager.getFirehoseItemCount(Mockito.anyBoolean(), Mockito.anyString())).thenReturn(4);

        recentItemsGet.process(element, jid, request, null);
        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.result, response.getType());
        Element pubsub = response.getChildElement();
        Assert.assertEquals("pubsub", pubsub.getName());
        Assert.assertEquals(JabberPubsub.NAMESPACE_URI,
                pubsub.getNamespaceURI());

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

        NodeItem item1 = new NodeItemImpl(TEST_NODE_1, "1", new Date(),
                "<entry>item1</entry>");
        NodeItem item2 = new NodeItemImpl(TEST_NODE_1, "2", new Date(),
                "<entry>item2");

        ArrayList<NodeItem> results = new ArrayList<NodeItem>();
        results.add(item1);
        results.add(item2);

        Mockito.when(
                channelManager.getFirehose(Mockito.anyInt(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyString())).thenReturn(
                new ClosableIteratorImpl<NodeItem>(results.iterator()));

        recentItemsGet.process(element, jid, request, null);
        IQ response = (IQ) queue.poll();
        Assert.assertEquals(1, response.getChildElement().element("items")
                .elements("item").size());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testCanControlGatheredEntriesUsingRsm() throws Exception {

        NodeItem item2 = new NodeItemImpl(TEST_NODE_2, "node2:1", new Date(),
                "<entry>item2</entry>");
        NodeItem item3 = new NodeItemImpl(TEST_NODE_1, "node1:2", new Date(),
                "<entry>item3</entry>");

        ArrayList<NodeItem> results = new ArrayList<NodeItem>();
        results.add(item2);
        results.add(item3);

        Mockito.when(
                channelManager.getFirehose(Mockito.anyInt(), Mockito.anyString(), 
                        Mockito.anyBoolean(), Mockito.anyString())).thenReturn(
                new ClosableIteratorImpl<NodeItem>(results.iterator()));
        Mockito.when(
                channelManager.getFirehoseItemCount(Mockito.anyBoolean(), 
                        Mockito.anyString())).thenReturn(2);

        Element rsm = request.getElement().addElement("rsm");
        rsm.addNamespace("", RecentItemsGet.NS_RSM);
        rsm.addElement("max").addText("2");
        rsm.addElement("after").addText("node1:1");

        recentItemsGet.process(element, jid, request, null);
        IQ response = (IQ) queue.poll();

        Assert.assertEquals(IQ.Type.result, response.getType());
        Element pubsub = response.getChildElement();
        Assert.assertEquals("pubsub", pubsub.getName());
        Assert.assertEquals(JabberPubsub.NAMESPACE_URI,
                pubsub.getNamespaceURI());

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
    
    @Test 
    public void testAdminUsersHaveRequestsMadeAsExpected() throws Exception {

        ArrayList<NodeItem> results = new ArrayList<NodeItem>();

        Mockito.when(
                channelManager.getFirehose(Mockito.anyInt(), Mockito.anyString(), Mockito.eq(true), 
                        Mockito.anyString())).thenReturn(
                new ClosableIteratorImpl<NodeItem>(results.iterator()));

        recentItemsGet.process(element, jid, request, null);
    }
}
