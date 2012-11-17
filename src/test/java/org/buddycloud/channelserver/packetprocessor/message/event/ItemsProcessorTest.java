package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.junit.Before;
import org.mockito.Mockito;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class ItemsProcessorTest extends IQTestHandler {
	private Message message;
	private ItemsProcessor itemsProcessor;

	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		Properties conf = new Properties();
		channelManager = Mockito.mock(ChannelManager.class);
		itemsProcessor = new ItemsProcessor(queue, conf, channelManager);
	}

}
