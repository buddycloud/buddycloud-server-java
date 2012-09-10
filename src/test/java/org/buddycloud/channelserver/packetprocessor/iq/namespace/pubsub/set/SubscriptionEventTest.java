package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.node.configuration.HelperMock;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelTitle;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class SubscriptionEventTest extends IQTestHandler {
	private IQ request;
	private Mock dataStore;
	private SubscriptionEvent event;
	private JID jid;
	private Element element;
	private String node;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception {
		dataStore = new Mock();
		queue = new LinkedBlockingQueue<Packet>();
		event = new SubscriptionEvent(queue, dataStore);
		jid = new JID("juliet@shakespeare.lit");
		request = readStanzaAsIq("/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza");
		node = "/user/pamela@denmark.lit/posts";

		event.setServerDomain("shakespeare.lit");

		element = new BaseElement("subscriptions");
		element.addAttribute("node", node);
	}

	@Test
	public void testPassingSubscriptionsAsElementNameReturnsTrue() {
		Element element = new BaseElement("subscriptions");
		assertTrue(event.accept(element));
	}

	@Test
	public void testPassingNotCreateAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-create");
		assertFalse(event.accept(element));
	}

	@Test
	public void testNotProvidingNodeAttributeReturnsErrorStanza()
			throws Exception {
		BaseElement element = new BaseElement("subscriptions");
		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}

	@Test
	public void testNotProvidingSubscriptionChildNodeReturnsErrorStanza()
			throws Exception {
		
		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza")
				.replaceFirst(
						"<subscription jid='francisco@denmark.lit' subscription='subscribed'/>",
						""));

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}

	@Test
	public void testNotProvidingJidAttributeReturnsErrorStanza()
			throws Exception {
		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/subscribe/authorizationPendingGrantreply.stanza")
				.replaceFirst("jid='francisco@denmark.lit'", ""));
		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}

	@Test
	public void testNotProvidingSubscriptionAttributeReturnsErrorStanza()
			throws Exception {
		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza")
				.replaceFirst("subscription='subscribed'", ""));
		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}

	@Test
	public void testDataStoreExceptionResultsInInternalServerErrorStanza()
			throws Exception {
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenThrow(
				DataStoreException.class);
		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.wait, error.getType());
		assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testNonExistantNodeRetunsErrorStanza() throws Exception {
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(false);
		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.cancel, error.getType());
		assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}

	@Test
	public void testUserWithoutAffiliationReturnsErrorStanza() throws Exception {
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(), node))
				.thenReturn(null);
		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.auth, error.getType());
		assertEquals(PacketError.Condition.not_authorized, error.getCondition());
	}

	@Test
	public void testUserWhoIsntOwnerOrModeratorCantUpdateSubscription()
			throws Exception {
		NodeSubscriptionImpl subscriptionMock = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMock.getAffiliation())
				.thenReturn("subscribed");

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(), node))
				.thenReturn(subscriptionMock);
		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.auth, error.getType());
		assertEquals(PacketError.Condition.not_authorized, error.getCondition());
	}

	@Test
	public void testSubscribingUserHasExistingSubscriptionToUpdate()
			throws Exception {
		NodeSubscriptionImpl subscriptionMock = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMock.getAffiliation())
				.thenReturn("subscribed");

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(), node))
				.thenReturn(subscriptionMock);
		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.auth, error.getType());
		assertEquals(PacketError.Condition.not_authorized, error.getCondition());
	}
}