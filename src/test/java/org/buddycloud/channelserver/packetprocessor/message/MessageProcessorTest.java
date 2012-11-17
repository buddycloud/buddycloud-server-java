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

	@Test(expected=NullPointerException.class)
	public void testNoEventElementPresentThrowsException() throws Exception {
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

/**
 * <message from="channels.surevine.com" type="headline"
 * to="channels.ip-10-66-2-93"><event
 * xmlns="http://jabber.org/protocol/pubsub#event"><items
 * node="/user/lounge@topics.surevine.com/posts"><item
 * id="17fb16fa-defe-4a5e-b85f-c57a91019e51"><entry
 * xmlns="http://www.w3.org/2005/Atom"><content>making a post
 * here.</content><author
 * ><name>lloyd.watkin@surevine.com</name><uri>acct:lloyd.watkin
 * @surevine.com</uri></author><in-reply-to
 * xmlns="http://purl.org/syndication/thread/1.0"
 * ref="d2c8ec95-61f1-4f12-ba2c-7038044751eb"
 * /><id>17fb16fa-defe-4a5e-b85f-c57a91019e51
 * </id><published>2012-11-17T15:41:17.874
 * Z</published><updated>2012-11-17T15:41:17.874Z</updated><link rel="self"
 * href=
 * "xmpp:channels.surevine.com?pubsub;action=retrieve;node=/user/lounge@topics.surevine.com/posts;item=17fb16fa-defe-4a5e-b85f-c57a91019e51"
 * /><verb xmlns="http://activitystrea.ms/spec/1.0/">comment</verb><object
 * xmlns=
 * "http://activitystrea.ms/spec/1.0/"><object-type>comment</object-type></
 * object></entry></item></items></event></message>
 */