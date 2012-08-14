package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import static org.junit.Assert.fail;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;


import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeCreate;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.Packet;
import org.xmpp.packet.IQ;

public class NodeCreateTest
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
}