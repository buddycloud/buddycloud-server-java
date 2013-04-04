package org.buddycloud.channelserver.packetprocessor.message;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import junit.framework.Assert;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class MessageProcessorTest extends IQTestHandler {
	private Message message;
	private MessageProcessor messageProcessor;

	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		Properties conf = new Properties();
		channelManager = Mockito.mock(ChannelManager.class);
		messageProcessor = new MessageProcessor(queue, conf, channelManager);
	}
	
	@Test
	public void testNonHeadlineEventPerformsNoAction() throws Exception {
		Message message = new Message();
		message.setType(Message.Type.chat);
		
		messageProcessor.process(message);
		Assert.assertEquals(0, queue.size());
	}

	@Test(expected=UnsupportedOperationException.class)
	public void testNoEventOrDataFormElementPresentThrowsException() throws Exception {
		Message message = new Message();
		message.setType(Message.Type.headline);
		
		messageProcessor.process(message);
	}
	
	@Test(expected=UnknownEventContentException.class)
	public void testUnknownEventContentTypeThrowsException() throws Exception {
		Message message = new Message();
		message.setType(Message.Type.headline);
		Element event = message.addChildElement("event", JabberPubsub.NS_PUBSUB_EVENT);
		event.addElement("random");
		
		messageProcessor.process(message);
	}
}