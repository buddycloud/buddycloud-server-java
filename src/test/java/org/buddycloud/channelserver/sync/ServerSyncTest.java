package org.buddycloud.channelserver.sync;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.Packet;

public class ServerSyncTest extends IQTestHandler {

	private BlockingQueue<Packet> outQueue = new LinkedBlockingQueue<Packet>();
	private BlockingQueue<Packet> inQueue = new LinkedBlockingQueue<Packet>();
	private ChannelManagerFactory channelManagerFactory;
	private ServerSync serverSync;

	@Before
	public void setUp() throws Exception {

		channelManagerFactory = Mockito.mock(ChannelManagerFactory.class);
		serverSync = new ServerSync(channelManagerFactory, outQueue, inQueue);
	}

	@Test
	public void testChannelManagerIsCreated() {
		
		Mockito.verify(channelManagerFactory, Mockito.times(1)).create();
	}
}