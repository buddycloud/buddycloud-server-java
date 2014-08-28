package org.buddycloud.channelserver.queue;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.ChannelsEngine;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.utils.users.OnlineResourceManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class OutQueueConsumerTest {
	private static final String SERVER_DOMAIN = "server1";
	private static final String SERVER_CHANNELS_DOMAIN = "channels.server1";
	private static final String SERVER_TOPICS_DOMAIN = "topics.server1";
	
	/*
	 * Class under test
	 */
	private OutQueueConsumer consumer;

	@Mock
	private ChannelsEngine channelsEngine;

	@Mock
	private BlockingQueue<Packet> outQueue;

	@Mock
	private FederatedQueueManager federatedQueueManager;

	@Mock
	private Configuration configuration;

	@Mock
	private OnlineResourceManager onlineResourceManager;
	
	@Mock
	private BlockingQueue<Packet> inQueue;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		
		consumer = new OutQueueConsumer(channelsEngine, outQueue,
				federatedQueueManager, configuration, onlineResourceManager, inQueue);

		when(configuration.getServerDomain()).thenReturn(SERVER_DOMAIN);
		when(configuration.getServerChannelsDomain()).thenReturn(SERVER_CHANNELS_DOMAIN);
		when(configuration.getServerTopicsDomain()).thenReturn(SERVER_TOPICS_DOMAIN);
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testConsumeForRemoteServerRequestForDiscover() throws Exception {
		JID remoteServer = new JID("server2");
		
		Message message = new Message();
		
		message.setTo(remoteServer);
		message.getElement().addAttribute("remote-server-discover", "true");
		
		consumer.consume(message);
		
		verify(federatedQueueManager).addChannelMap(remoteServer);
	}

	@Test
	public void testConsumeForRemoteServerRequestForDelivery() throws Exception {
		JID remoteServer = new JID("server2");
		
		Message message = new Message();
		
		message.setTo(remoteServer);
		
		consumer.consume(message);
		
		verify(federatedQueueManager).process(message);
	}

	@SuppressWarnings("serial")
	@Test
	public void testConsumeForRemoteUser() throws Exception {
		JID jid = new JID("user1@server2");
		final JID resource1 = new JID("user1@server2/resource1");
		final JID resource2 = new JID("user1@server2/resource2");
		
		when(onlineResourceManager.getResources(jid)).thenReturn(new ArrayList<JID>() {{
			add(resource1);
			add(resource2);
		}});
		
		Message message = new Message();
		
		message.setTo(jid);
		
		consumer.consume(message);
		
		ArgumentCaptor<Packet> packetCaptor = ArgumentCaptor.forClass(Packet.class);
		
		Message expected1 = message.createCopy();
		expected1.setTo(resource1);		
		verify(channelsEngine, times(2)).sendPacket(packetCaptor.capture());
		assertEquals("Packet not sent to " + resource1, resource1, packetCaptor.getAllValues().get(0).getTo());
		assertEquals("Packet not sent to " + resource2, resource2, packetCaptor.getAllValues().get(1).getTo());
	}
	
	public void testConsumeForLocalServer() throws Exception {
		JID jid = new JID(SERVER_DOMAIN);
		
		Message message = new Message();
		
		message.setTo(jid);
		
		consumer.consume(message);
		
		verify(channelsEngine).sendPacket(message);
	}
	
	public void testConsumeForLocalChannelsServer() throws Exception {
		JID jid = new JID(SERVER_CHANNELS_DOMAIN);
		
		Message message = new Message();
		
		message.setTo(jid);
		
		consumer.consume(message);
		
		verify(channelsEngine).sendPacket(message);
	}
	
	public void testConsumeForLocalTopicsServer() throws Exception {
		JID jid = new JID(SERVER_TOPICS_DOMAIN);
		
		Message message = new Message();
		
		message.setTo(jid);
		
		consumer.consume(message);
		
		verify(channelsEngine).sendPacket(message);
	}
}
