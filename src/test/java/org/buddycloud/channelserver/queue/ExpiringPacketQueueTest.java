package org.buddycloud.channelserver.queue;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.ChannelsEngine;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.buddycloud.channelserver.channel.ChannelsEngineMock;
import org.dom4j.Element;
import org.dom4j.tree.DefaultElement;

public class ExpiringPacketQueueTest extends IQTestHandler {

	private ExpiringPacketQueue queue;

	@Before
	public void setUp() throws Exception {
		queue = new ExpiringPacketQueue();
	}

	@Test
	public void testShortExpiryLeadsToPacketsExpiring() throws Exception {
		queue.setTimeout(1);
		queue.start();
		queue.put("packet:1", new IQ());
		Thread.sleep(15);
		Assert.assertEquals(0, queue.size());
	}
	
	@Test
	public void testLongExpiryLeadsToPacketsNotExpiring() throws Exception {
		queue.setTimeout(100);
		queue.start();
		queue.put("packet:1", new IQ());
		Thread.sleep(2);
		Assert.assertEquals(1, queue.size());
	}
	
	@Test
	public void testDoesNotFailIfPacketRemovedEarly() throws Exception {
		queue.setTimeout(100);
		queue.start();
		queue.put("packet:1", new IQ());
		queue.put("packet:2", new IQ());
		queue.remove("packet:1");
		Assert.assertEquals(1, queue.size());	
		Assert.assertNull(queue.get("packet:1"));
		Assert.assertNotNull(queue.get("packet:2"));
	}
	
	@Test
	public void testDoesNotFailOnExpireIfPacketRemovedEarly() throws Exception {
		queue.setTimeout(1);
		queue.start();
		queue.put("packet:1", new IQ());
		queue.put("packet:2", new IQ());
		queue.remove("packet:1");
		Thread.sleep(1);
		queue.expireEntries();
		Assert.assertEquals(0, queue.size());	
	}
}