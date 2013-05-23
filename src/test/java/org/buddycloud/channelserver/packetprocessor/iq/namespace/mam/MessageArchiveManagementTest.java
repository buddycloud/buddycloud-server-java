package org.buddycloud.channelserver.packetprocessor.iq.namespace.mam;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerImpl;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class MessageArchiveManagementTest extends IQTestHandler {

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;
	private MessageArchiveManagement mam;
	private LinkedBlockingQueue<Packet> queue;
	private IQ request;

	@Before
	public void setUp() throws Exception {
		channelManager = Mockito.mock(ChannelManagerImpl.class);
		queue = new LinkedBlockingQueue<Packet>();
		mam = new MessageArchiveManagement(queue, channelManager);

		request = readStanzaAsIq("/iq/mam/request.stanza");

		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);
	}

	@Test
	public void testRequestsOnlyHonouredForLocalUsers() throws Exception {
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(false);

		mam.process(request);

		Assert.assertEquals(1, queue.size());

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.service_unavailable,
				error.getCondition());
	}

	@Test
	public void testInvalidStartTimestampResultsInBadRequestStanza()
			throws Exception {

		request.getChildElement().element("start").setText("not-a-date");
		mam.process(request);

		Assert.assertEquals(1, queue.size());

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testInvalidEndTimestampResultsInBadRequestStanza()
			throws Exception {

		request.getChildElement().element("end").setText("not-a-date");
		mam.process(request);

		Assert.assertEquals(1, queue.size());

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}
	
	//@Test
	//public void testNoNotificationsResultsInJustResultPacket() throws Exception {

	//}
}