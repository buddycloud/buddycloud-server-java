package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class PublishTest extends IQTestHandler {
	
	private IQ request;
	private ChannelManager channelManager;
	private Publish publish;
	private JID jid;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private String node = "/user/romeo@shakespeare.lit/posts";

	@Before
	public void setUp() throws Exception {
		channelManager= Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);

		queue = new LinkedBlockingQueue<Packet>();
		publish = new Publish(queue, channelManager);
		jid = new JID("juliet@shakespeare.lit");
		request = readStanzaAsIq("/iq/pubsub/publish/request.stanza");

		publish.setServerDomain("shakespeare.lit");
		publish.setChannelManager(channelManager);

		element = new BaseElement("publish");
		
		Mockito.when(channelManager.isLocalNode(node)).thenReturn(true);
	}

	@Test
	public void testPassingRetractAsElementNameReturnsTrue() {
		Element element = new BaseElement("publish");
		Assert.assertTrue(publish.accept(element));
	}

	@Test
	public void testPassingNotRetractAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-publish");
		Assert.assertFalse(publish.accept(element));
	}

	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception {
		
		IQ request = this.request.createCopy();
		request.getChildElement().element("publish").attribute("node").detach();
		
		publish.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required",
				error.getApplicationConditionName());
	}

	@Test
	public void testNodeStoreExceptionReturnsErrorStanza() throws Exception {
		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.nodeExists(Mockito.eq(node));

		publish.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
		Assert.assertEquals(PacketError.Type.wait, error.getType());

	}

	@Test
	public void testProvidingNodeWhichDoesntExistReturnsError()
			throws Exception {
		Mockito.when(channelManager.nodeExists(node)).thenReturn(false);

		publish.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}
	
	@Test
	public void testRequestToRemoteNodeResultsInForwardedPacket() throws Exception {
		Mockito.when(channelManager.isLocalNode(node)).thenReturn(false);
		
		Assert.assertEquals(new JID("channels.shakespeare.lit"), request.getTo());
		
		publish.process(element, jid, request, null);

		Assert.assertEquals(1, queue.size());
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
		Assert.assertEquals(new JID("shakespeare.lit"), response.getTo());
	}

}