package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import static org.junit.Assert.fail;
import org.dom4j.Element;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;


import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeCreate;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.Packet;
import org.xmpp.packet.IQ;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;

public class NodeCreateTest extends IQHandlerTest
{
	private IQ         stanza;
	private DataStore  dataStore;
	private NodeCreate nodeCreate;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception
	{
		dataStore  = new Mock();
		queue      = new LinkedBlockingQueue<Packet>();
		nodeCreate = new NodeCreate(queue, dataStore);
		stanza     = readStanzaAsIq("/iq/pubsub/channel/create/request.stanza");
	}

	@Test
	public void testPassingCreateAsElementNameReturnsTrue()
	{
		// @todo This is ugly, there's surely a better way?
		Element element = stanza.getChildElement();
		IQ iq = new IQ(element);
		
		assertTrue(nodeCreate.accept(iq.getChildElement()));
	}
	
	@Test
	public void testPassingNotCreateAsElementNameReturnsFalse()
	{
		Element element = stanza.getChildElement();
		assertFalse(nodeCreate.accept(element));
	}
}