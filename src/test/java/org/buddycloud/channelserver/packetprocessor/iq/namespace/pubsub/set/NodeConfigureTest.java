package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeConfigureTest extends IQHandlerTest
{
	private IQ            request;
	private Mock          dataStore;
	private NodeConfigure nodeConfigure;
	private JID           jid;
	private Element       element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception
	{
		dataStore     = new Mock();
		queue         = new LinkedBlockingQueue<Packet>();
	    nodeConfigure = new NodeConfigure(queue, dataStore);
		jid           = new JID("juliet@shakespeare.lit");
		request       = readStanzaAsIq("/iq/pubsub/channel/configure/request.stanza");
		
		nodeConfigure.setServerDomain("shakespeare.lit");
		
		element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespeare.lit/posts");
	}

	@Test
	public void testPassingConfigureAsElementNameReturnsTrue()
	{
		Element element = new BaseElement("configure");
		assertTrue(nodeConfigure.accept(element));
	}
	
	@Test
	public void testPassingNotConfigureAsElementNameReturnsFalse()
	{
		Element element = new BaseElement("not-configure");
		assertFalse(nodeConfigure.accept(element));
	}
	
	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception
	{
		Element element = new BaseElement("configure");
		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}
	
	@Test
	public void testNonExistantNodeReturnsErrorStanza() throws Exception
	{
		Element element = new BaseElement("configure");
	    element.addAttribute("node", "/user/not-here@shakespeare.lit/status");
	    
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito
		    .when(dataStoreMock.nodeExists("/user/not-here@shakespeare.lit/status"))
		    .thenReturn(false);
        nodeConfigure.setDataStore(dataStoreMock);
        System.out.println(element.asXML());
        nodeConfigure.process(element, jid, request, null);
	    Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
	    PacketError error = response.getError();
	    assertNotNull(error);
	    assertEquals(PacketError.Type.cancel, error.getType());
	    assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}
}