package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoitems;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerImpl;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class DiscoItemsGetTest extends IQTestHandler {

    private ChannelManager channelManager;
    private FederatedQueueManager federatedQueueManager;
    private DiscoItemsGet discoItems;
    private LinkedBlockingQueue<Packet> queue;

    private IQ request;
    private IQ requestWithNode;

    @Before
    public void setUp() throws Exception {
        channelManager = Mockito.mock(ChannelManagerImpl.class);
        federatedQueueManager = Mockito.mock(FederatedQueueManager.class);
        queue = new LinkedBlockingQueue<Packet>();

        discoItems = new DiscoItemsGet(queue, channelManager, federatedQueueManager);

        request = readStanzaAsIq("/iq/discoitems/request.stanza");
        requestWithNode = readStanzaAsIq("/iq/discoitems/requestWithNode.stanza");
    }

    @Test
    public void testReturnsListOfNodes() throws Exception {
        ArrayList<String> nodes = new ArrayList<String>();
        nodes.add("/user/user1@server1.com/posts");
        nodes.add("/user/topic@topics.server1.com/posts");
        nodes.add("/user/user2@server1.com/posts");

        Mockito.when(channelManager.getNodeList()).thenReturn(nodes);
        Mockito.when(channelManager.isLocalNode(Mockito.anyString())).thenReturn(true);

        discoItems.process(request);

        Assert.assertEquals(1, queue.size());
        Packet iq = queue.poll();

        String jid = Configuration.getInstance().getProperty(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN);

        @SuppressWarnings("unchecked")
        List<Element> items = iq.getElement().element("query").elements("item");
        Assert.assertEquals(3, items.size());
        Assert.assertEquals(jid, items.get(0).attributeValue("jid"));
        Assert.assertEquals("/user/user1@server1.com/posts", items.get(0).attributeValue("node"));
        Assert.assertEquals(jid, items.get(1).attributeValue("jid"));
        Assert.assertEquals("/user/topic@topics.server1.com/posts", items.get(1).attributeValue("node"));
    }

    @Test
    public void testOnlyReturnsLocalNodes() throws Exception {
        ArrayList<String> nodes = new ArrayList<String>();
        nodes.add("/user/user1@server1.com/posts");
        nodes.add("/user/topic@topics.server1.com/posts");
        nodes.add("/user/user2@server1.com/posts");

        Mockito.when(channelManager.getNodeList()).thenReturn(nodes);
        Mockito.when(channelManager.isLocalNode(Mockito.anyString())).thenReturn(false);

        discoItems.process(request);

        Assert.assertEquals(1, queue.size());
        Packet iq = queue.poll();
        List<Element> items = iq.getElement().element("query").elements("item");
        Assert.assertEquals(0, items.size());
    }

    @Test
    public void testReturnsErrorIfDataStoreException() throws Exception {
        Mockito.when(channelManager.getNodeList()).thenThrow(new NodeStoreException());

        discoItems.process(request);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.wait, error.getType());
        Assert.assertEquals(PacketError.Condition.internal_server_error, error.getCondition());
    }

    /* Not supporting this yet */
    @Test
    public void testReturnsErrorIfNodeProvided() throws Exception {

        discoItems.process(requestWithNode);
        Packet response = queue.poll();

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.cancel, error.getType());
        Assert.assertEquals(PacketError.Condition.feature_not_implemented, error.getCondition());
    }
}
