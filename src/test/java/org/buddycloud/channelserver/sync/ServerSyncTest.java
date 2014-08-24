package org.buddycloud.channelserver.sync;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
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
	private ChannelManager channelManager;
	private ServerSync serverSync;
	private Configuration configuration;

	@Before
	public void setUp() throws Exception {

		configuration = Mockito.mock(Configuration.class);

		channelManager = Mockito.mock(ChannelManager.class);
		channelManagerFactory = Mockito.mock(ChannelManagerFactory.class);
		Mockito.when(channelManagerFactory.create()).thenReturn(channelManager);
		serverSync = new ServerSync(channelManagerFactory, outQueue, inQueue,
				configuration);
	}

	@Test
	public void testChannelManagerIsCreated() {

		Mockito.verify(channelManagerFactory, Mockito.times(1)).create();
	}

	@Test
	public void ifUserChoosesToPurgeRemoteDataThenMethodIsCalled()
			throws Exception {

		Mockito.when(
				configuration.getProperty(
						Mockito.eq(Configuration.PURGE_REMOTE_ON_START),
						Mockito.eq("false"))).thenReturn("true");
		serverSync.start();
		Mockito.verify(channelManager, Mockito.times(1)).deleteRemoteData();
	}

	@Test
	public void ifUserChoosesNotToPurgeRemoteDataThenMethodIsNotCalled()
			throws Exception {
		Mockito.when(
				configuration.getProperty(
						Mockito.eq(Configuration.PURGE_REMOTE_ON_START),
						Mockito.eq("false"))).thenReturn("false");
		serverSync.start();
		Mockito.verify(channelManager, Mockito.times(0)).deleteRemoteData();
	}

	@Test
	public void ifNoChoiceIsMadeAboutPurgingRemoteDataThenMethodIsNotCalled()
			throws Exception {
		Mockito.when(
				configuration.getProperty(
						Mockito.eq(Configuration.PURGE_REMOTE_ON_START),
						Mockito.eq("false"))).thenReturn(null);
		serverSync.start();
		Mockito.verify(channelManager, Mockito.times(0)).deleteRemoteData();
	}

}