package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import static org.junit.Assert.fail;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;


import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeCreate;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.Packet;

public class NodeCreateTest 
{
	private DataStore  dataStore;
	private NodeCreate nodeCreate;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception
	{
		dataStore  = new Mock();
		queue      = new LinkedBlockingQueue<Packet>();
		nodeCreate = new NodeCreate(this.queue, this.dataStore);
	}

	@Test
	public void test()
	{
		fail("Not yet implemented");
	}

}
