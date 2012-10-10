package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.node.configuration.HelperMock;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelTitle;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class ItemDeleteTest extends IQTestHandler
{
	private IQ         request;
	private Mock       channelManagerMock;
	private ItemDelete itemDelete;
	private JID        jid;
	private Element    element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
    private String     node = "/user/capulet@shakespeare.lit/posts";
    
	@Before
	public void setUp() throws Exception
	{
		channelManagerMock = Mockito.mock(Mock.class);
        
		queue      = new LinkedBlockingQueue<Packet>();
		itemDelete = new ItemDelete(queue, channelManagerMock);
		jid        = new JID("juliet@shakespeare.lit");
		request    = readStanzaAsIq("/iq/pubsub/item/delete/request.stanza");
		
		itemDelete.setServerDomain("shakespeare.lit");
		itemDelete.setChannelManager(channelManagerMock);
		
		element = new BaseElement("retract");
		element.addAttribute("node", node);
	}

	@Test
	public void testPassingRetractAsElementNameReturnsTrue()
	{
		Element element = new BaseElement("retract");
		assertTrue(itemDelete.accept(element));
	}
	
	@Test
	public void testPassingNotRetractAsElementNameReturnsFalse()
	{
		Element element = new BaseElement("not-retract");
		assertFalse(itemDelete.accept(element));
	}
	
	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception
	{
		Element element = new BaseElement("retract");
		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}
	
	@Test
	public void testNodeStoreExceptionReturnsErrorStanza() throws Exception {
		Mockito
		    .when(channelManagerMock.isLocalNode(node))
		    .thenReturn(true);
	    Mockito
	        .doThrow(new NodeStoreException())
	        .when(channelManagerMock).nodeExists(node);

		itemDelete.process(element, jid, request, null);
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Condition.internal_server_error, error.getCondition());
		assertEquals(PacketError.Type.wait, error.getType());
		
	}

	@Test
	public void testNonLocalNodeReturnsError()
        throws Exception
	{
		String node = "/user/capulet@marlowe.lit/posts";
		element.addAttribute("node", node);
		Mockito
		    .when(channelManagerMock.isLocalNode(node))
		    .thenReturn(false);
        itemDelete.setChannelManager(channelManagerMock);

		itemDelete.process(element, jid, request, null);
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.cancel, error.getType());
		assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}
	
	@Test
	public void testProvidingNodeWhichDoesntExistReturnsError()
        throws Exception
	{
		Mockito
		    .when(channelManagerMock.nodeExists("/user/capulet@shakespeare.lit/posts"))
		    .thenReturn(false);
        itemDelete.setChannelManager(channelManagerMock);

		itemDelete.process(element, jid, request, null);
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.cancel, error.getType());
		assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}
}