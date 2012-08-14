package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import static org.junit.Assert.fail;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.dom4j.tree.BaseElement;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;


import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeCreate;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.IQ;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;

public class NodeCreateTest extends IQHandlerTest
{
	private IQ         stanza;
	private DataStore  dataStore;
	private NodeCreate nodeCreate;
	private JID        jid;
	private IQ         request;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception
	{
		dataStore  = new Mock();
		queue      = new LinkedBlockingQueue<Packet>();
		nodeCreate = new NodeCreate(queue, dataStore);
		jid        = new JID("juliet@capulet.lit");
		request    = readStanzaAsIq("/iq/pubsub/channel/create/request.stanza");
		
	}

	@Test
	public void testPassingCreateAsElementNameReturnsTrue()
	{
		Element element = new BaseElement("create");
		assertTrue(nodeCreate.accept(element));
	}
	
	@Test
	public void testPassingNotCreateAsElementNameReturnsFalse()
	{
		Element element = new BaseElement("not-create");
		assertFalse(nodeCreate.accept(element));
	}
	
	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception
	{
		Element element = new BaseElement("create");
		nodeCreate.process(element, jid, request, null);

		    Packet response = queue.poll(1, TimeUnit.MILLISECONDS);
		    PacketError error           = response.getError();

		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}
}