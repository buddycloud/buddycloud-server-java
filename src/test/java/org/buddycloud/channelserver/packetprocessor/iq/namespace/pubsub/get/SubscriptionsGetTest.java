package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class SubscriptionsGetTest extends IQTestHandler {

	private IQ request;
	private SubscriptionsGet subscriptionsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		subscriptionsGet = new SubscriptionsGet(queue, channelManager);
		element = new BaseElement("subscriptions");

		channelManager = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		subscriptionsGet.setChannelManager(channelManager);
	}

	@Test
	public void testPassingSubscriptionsAsElementNameReturnsTrue() {
		Assert.assertTrue(subscriptionsGet.accept(element));
	}

	@Test
	public void testPassingNotSubscriptionsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-subscriptions");
		Assert.assertFalse(subscriptionsGet.accept(element));
	}
}