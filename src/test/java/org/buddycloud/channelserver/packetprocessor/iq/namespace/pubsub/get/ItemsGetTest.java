package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.ItemsGet;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class ItemsGetTest extends IQTestHandler {

	private IQ request;
	private ItemsGet itemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		itemsGet = new ItemsGet(queue, channelManager);
		request = readStanzaAsIq("/iq/pubsub/items/request.stanza");
		element = new BaseElement("items");

		channelManager = Mockito.mock(ChannelManager.class);
		itemsGet.setChannelManager(channelManager);
	}

	@Test
	public void testPassingItemsAsElementNameReturnsTrue() {
		Assert.assertTrue(itemsGet.accept(element));
	}

	@Test
	public void testPassingNotItemsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		Assert.assertFalse(itemsGet.accept(element));
	}

	@Test
	public void testMissingNodeAttributeReturnsErrorStanza() throws Exception {
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required",
				error.getApplicationConditionName());
	}
}