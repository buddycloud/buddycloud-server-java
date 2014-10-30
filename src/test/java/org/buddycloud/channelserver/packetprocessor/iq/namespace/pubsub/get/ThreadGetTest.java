package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
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

public class ThreadGetTest extends IQTestHandler {

    private static final Integer TOTAL_RESULTS = 4;
    private IQ request;
    private PubSubElementProcessorAbstract threadGet;
    private Element element;
    private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

    private String node = "/user/pamela@denmark.lit/posts";
    private JID jid = new JID("juliet@shakespeare.lit");
    private ChannelManager channelManager;

    private static final String TEST_NODE = "node1";
    private NodeViewAcl nodeViewAclMock;

    @Before
    public void setUp() throws Exception {

        queue = new LinkedBlockingQueue<Packet>();
        channelManager = Mockito.mock(ChannelManager.class);

        threadGet = new ThreadGet(queue, channelManager);

        request = readStanzaAsIq("/iq/pubsub/thread/request.stanza");
        element = new BaseElement("thread");

        Mockito.when(channelManager.nodeExists(Mockito.anyString())).thenReturn(true);
        Mockito.doReturn(new HashMap<String, String>()).when(channelManager).getNodeConf(Mockito.anyString());
        Mockito.when(channelManager.getCountNodeThread(Mockito.anyString(), Mockito.anyString())).thenReturn(TOTAL_RESULTS);
        Mockito.when(channelManager.getNodeItem(Mockito.anyString(), Mockito.anyString())).thenReturn(
                new NodeItemImpl(TEST_NODE, "1", new Date(), "payload"));

        nodeViewAclMock = Mockito.mock(NodeViewAcl.class);

        threadGet.setNodeViewAcl(nodeViewAclMock);

        Mockito.doReturn(true).when(nodeViewAclMock)
                .canViewNode(Mockito.anyString(), Mockito.any(NodeMembershipImpl.class), Mockito.any(AccessModels.class), Mockito.anyBoolean());
    }

    @Test
    public void testPassingRecentItemsAsElementNameReturnsTrue() {
        Assert.assertTrue(threadGet.accept(element));
    }

    @Test
    public void testPassingNotThreadAsElementNameReturnsFalse() {
        Element element = new BaseElement("not-thread");
        Assert.assertFalse(threadGet.accept(element));
    }

    @Test
    public void testMissingNodeAttributeReturnsErrorStanza() throws Exception {

        request.getChildElement().element("thread").addAttribute("node", null);

        threadGet.process(element, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals("nodeid-required", error.getApplicationConditionName());
    }

    @Test
    public void testMissingItemIdAttributeReturnsErrorStanza() throws Exception {

        request.getChildElement().element("thread").addAttribute("item_id", null);

        threadGet.process(element, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals("itemid-required", error.getApplicationConditionName());
    }

    @Test
    public void testNodeStoreExceptionGeneratesAnErrorStanza() throws Exception {

        Mockito.when(channelManager.nodeExists(Mockito.anyString())).thenThrow(new NodeStoreException());

        threadGet.process(element, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.wait, error.getType());
        Assert.assertEquals(PacketError.Condition.internal_server_error, error.getCondition());
    }

    @Test
    public void userWhoCantAccessChannelGetsPermissionErrorStanzaReply() throws Exception {

        Mockito.doReturn(false).when(nodeViewAclMock)
                .canViewNode(Mockito.anyString(), Mockito.any(NodeMembershipImpl.class), Mockito.any(AccessModels.class), Mockito.anyBoolean());
        NodeAclRefuseReason refusalReason = new NodeAclRefuseReason(PacketError.Type.auth, PacketError.Condition.forbidden, "pending-subscription");
        Mockito.when(nodeViewAclMock.getReason()).thenReturn(refusalReason);

        threadGet.process(element, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
    }

    @Test
    public void ifItemDoesNotExistErrorStanzaIsReturned() throws Exception {
        Mockito.when(channelManager.getNodeItem(Mockito.anyString(), Mockito.anyString())).thenReturn(null);

        threadGet.process(element, jid, request, null);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.cancel, error.getType());
        Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
        Assert.assertEquals("parent-item-not-found", error.getApplicationConditionName());
        Assert.assertEquals(Buddycloud.NS_ERROR, error.getApplicationConditionNamespaceURI());
    }

    @Test
    public void testOutgoingStanzaFormattedAsExpected() throws Exception {

        ArrayList<NodeItem> expectedResults = new ArrayList<NodeItem>();
        expectedResults.add(new NodeItemImpl(TEST_NODE, "1", new Date(), "<entry>value1</entry>"));
        expectedResults.add(new NodeItemImpl(TEST_NODE, "2", new Date(), "<entry>value2</entry>"));

        Mockito.when(channelManager.getNodeItemThread(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt())).thenReturn(
                new ClosableIteratorImpl<NodeItem>(expectedResults.iterator()));

        threadGet.process(element, jid, request, null);
        Packet response = queue.poll();

        Element items = response.getElement().element("pubsub").element("items");
        Assert.assertEquals("/user/channeluser@example.com/posts", items.attributeValue("node"));
        Assert.assertEquals(2, items.elements("item").size());
        Assert.assertEquals("1", items.element("item").attributeValue("id"));
        Assert.assertEquals("value1", items.element("item").elementText("entry"));
    }

    @Test
    public void testUnparsableItemEntriesAreSimplyIgnored() throws Exception {

        ArrayList<NodeItem> expectedResults = new ArrayList<NodeItem>();
        expectedResults.add(new NodeItemImpl(TEST_NODE, "1", new Date(), "<entry>value1</entry>"));
        expectedResults.add(new NodeItemImpl(TEST_NODE, "2", new Date(), "<entry>value2"));

        Mockito.when(channelManager.getNodeItemThread(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt())).thenReturn(
                new ClosableIteratorImpl<NodeItem>(expectedResults.iterator()));

        threadGet.process(element, jid, request, null);
        Packet response = queue.poll();

        Element items = response.getElement().element("pubsub").element("items");
        Assert.assertEquals("/user/channeluser@example.com/posts", items.attributeValue("node"));
        Assert.assertEquals(1, items.elements("item").size());
        Assert.assertEquals("1", items.element("item").attributeValue("id"));
        Assert.assertEquals("value1", items.element("item").elementText("entry"));
    }

    @Test
    public void testRsmElementIsAddedCorrectly() throws Exception {

        Element rsm = request.getChildElement().addElement("set");
        rsm.addNamespace("", PubSubElementProcessorAbstract.NS_RSM);
        rsm.addElement("max").setText("4");
        rsm.addElement("after").setText("1");

        ArrayList<NodeItem> expectedResults = new ArrayList<NodeItem>();
        expectedResults.add(new NodeItemImpl(TEST_NODE, "1", new Date(), "<entry>value1</entry>"));
        expectedResults.add(new NodeItemImpl(TEST_NODE, "2", new Date(), "<entry>value2</entry>"));
        expectedResults.add(new NodeItemImpl(TEST_NODE, "3", new Date(), "<entry>value3</entry>"));
        expectedResults.add(new NodeItemImpl(TEST_NODE, "4", new Date(), "<entry>value4</entry>"));

        Mockito.when(channelManager.getNodeItemThread(Mockito.anyString(), Mockito.anyString(), Mockito.eq("1"), Mockito.eq(4))).thenReturn(
                new ClosableIteratorImpl<NodeItem>(expectedResults.iterator()));

        threadGet.process(element, jid, request, null);
        Packet response = queue.poll();

        Element items = response.getElement().element("pubsub").element("items");

        Assert.assertEquals("/user/channeluser@example.com/posts", items.attributeValue("node"));
        Assert.assertEquals(4, items.elements("item").size());

        Element rsmResult = response.getElement().element("pubsub").element("set");
        Assert.assertEquals("1", rsmResult.elementText("first"));
        Assert.assertEquals("4", rsmResult.elementText("last"));
        Assert.assertEquals(String.valueOf(TOTAL_RESULTS), rsmResult.elementText("count"));
    }
}
