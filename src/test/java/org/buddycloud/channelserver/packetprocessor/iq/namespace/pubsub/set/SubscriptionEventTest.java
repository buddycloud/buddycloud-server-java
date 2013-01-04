package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscriptionMock;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SubscriptionEventTest extends IQTestHandler {
	private IQ request;
	private SubscriptionEvent event;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String subscriber = "francisco@denmark.lit";
	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private Mock dataStore;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		event = new SubscriptionEvent(queue, dataStore);
		request = readStanzaAsIq("/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza");
		event.setServerDomain("shakespeare.lit");

		element = new BaseElement("subscriptions");
		element.addAttribute("node", node);
		
		dataStore = Mockito.mock(Mock.class);
		Mockito.when(dataStore.isLocalNode(Mockito.anyString()))
		    .thenReturn(true);
		
		event.setChannelManager(dataStore);
	}

	@Test
	public void testPassingSubscriptionsAsElementNameReturnsTrue() {
		Element element = new BaseElement("subscriptions");
		Assert.assertTrue(event.accept(element));
	}

	@Test
	public void testPassingNotSubscriptionsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-subscriptions");
		Assert.assertFalse(event.accept(element));
	}

	@Test
	public void testNotProvidingNodeAttributeReturnsErrorStanza()
			throws Exception {
		BaseElement element = new BaseElement("subscriptions");
		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required", error.getApplicationConditionName());
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
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}

	@Test
	public void testNotProvidingJidAttributeReturnsErrorStanza()
			throws Exception {
		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza")
				.replaceFirst("jid='francisco@denmark.lit'", ""));
		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
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
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}

	@Test
	public void testNodeStoreExceptionResultsInInternalServerErrorStanza()
			throws Exception {
		Mockito.when(dataStore.nodeExists(Mockito.anyString())).thenThrow(
				NodeStoreException.class);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.wait, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testNonExistantNodeRetunsErrorStanza() throws Exception {

		Mockito.when(dataStore.nodeExists(node)).thenReturn(false);
		event.setChannelManager(dataStore);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}

	@Test
	public void testUserWithoutSubscriptionReturnsErrorStanza()
			throws Exception {

		Mockito.when(dataStore.nodeExists(node)).thenReturn(true);
		Mockito.when(dataStore.getUserSubscription(node, jid))
				.thenReturn(null);
		event.setChannelManager(dataStore);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized, error.getCondition());
	}

	@Test
	public void testUserWhoIsntOwnerOrModeratorCantUpdateSubscription()
			throws Exception {
		NodeAffiliation subscriptionMock = Mockito.mock(NodeAffiliation.class);
		Mockito.when(subscriptionMock.getAffiliation()).thenReturn(
				Affiliations.member);


		Mockito.when(dataStore.nodeExists(node)).thenReturn(true);
		Mockito.when(dataStore.getUserAffiliation(node, jid))
				.thenReturn(subscriptionMock);
		event.setChannelManager(dataStore);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized, error.getCondition());
	}

	@Test
	public void testSubscribingUserMustHaveExistingSubscriptionToUpdate()
			throws Exception {
		NodeAffiliation subscriptionMockActor = Mockito
				.mock(NodeAffiliation.class);
		Mockito.when(subscriptionMockActor.getAffiliation()).thenReturn(
				Affiliations.owner);


		Mockito.when(dataStore.nodeExists(node)).thenReturn(true);
		Mockito.when(dataStore.getUserAffiliation(node, jid))
				.thenReturn(subscriptionMockActor);
		Mockito.when(
				dataStore.getUserSubscription(node,
						new JID(subscriber))).thenReturn(null);
		event.setChannelManager(dataStore);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.unexpected_request,
				error.getCondition());
	}

	@Test
	@Ignore("Need to work out how to check set subscription values")
	public void testPassingInvalidSubscriptionTypeSetsSubscriptionToNone()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza")
				.replaceFirst("subscription='subscribed'",
						"subscription='i-can-haz-all-the-items'"));

		NodeAffiliation subscriptionMockActor = Mockito
				.mock(NodeAffiliation.class);
		Mockito.when(subscriptionMockActor.getAffiliation()).thenReturn(
				Affiliations.owner);

		NodeSubscription subscriptionMockSubscriber = Mockito
				.mock(NodeSubscription.class);
		Mockito.when(subscriptionMockSubscriber.getSubscription()).thenReturn(
				Subscriptions.subscribed);

		Mockito.when(dataStore.nodeExists(node)).thenReturn(true);
		Mockito.when(dataStore.getUserAffiliation(node, jid))
				.thenReturn(subscriptionMockActor);
		Mockito.doCallRealMethod().when(dataStore)
				.addUserSubscription(Mockito.any(NodeSubscription.class));
		Mockito.when(
				dataStore.getUserSubscription(node,
						new JID(subscriber))).thenReturn(
				subscriptionMockSubscriber);
	    
		event.setChannelManager(dataStore);
		event.process(element, jid, request, null);

		NodeSubscription subscriptionMock = new NodeSubscriptionImpl(node,
				new JID("francisco@denmark.lit"), new JID(
						"francisco@denmark.lit"), Subscriptions.none);
		
		/*
		 * subscriptionMock Mockito.anyString(), Mockito.any(JID.class),
		 * Mockito.any(JID.class), Mockito.eq(Subscriptions.none));
		 */
		Mockito.verify(dataStore)
				.addUserSubscription(subscriptionMock);
	}

	@Test
	public void testPassingValidSubscriptionSendsOutExpectedNotifications()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza")
				.replaceFirst("subscription='subscribed'",
						"subscription='subscribed'"));

		NodeAffiliation subscriptionMockActor = Mockito
				.mock(NodeAffiliation.class);
		Mockito.when(subscriptionMockActor.getAffiliation()).thenReturn(
				Affiliations.owner);

		NodeSubscription subscriptionMockSubscriber = Mockito
				.mock(NodeSubscription.class);
		Mockito.when(subscriptionMockSubscriber.getSubscription()).thenReturn(
				Subscriptions.subscribed);


		Mockito.when(dataStore.nodeExists(node)).thenReturn(true);
		Mockito.when(dataStore.getUserAffiliation(node, jid))
				.thenReturn(subscriptionMockActor);

		Mockito.when(
				dataStore.getUserSubscription(node,
						new JID(subscriber))).thenReturn(
				subscriptionMockSubscriber);

		event.setChannelManager(dataStore);

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionMock(new JID(
				"romeo@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"hamlet@shakespeare.lit")));

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers)).when(dataStore)
				.getNodeSubscriptionListeners(Mockito.anyString());

		event.setChannelManager(dataStore);
		event.process(element, jid, request, null);

		Assert.assertEquals(2, queue.size());
		Packet notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("romeo@shakespeare.lit", notification.getTo().toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("hamlet@shakespeare.lit", notification.getTo().toString());

		Assert.assertEquals(
				node,
				notification.getElement().element("event")
						.element("subscription").attributeValue("node"));
		Assert.assertTrue(notification.toXML().contains(JabberPubsub.NS_PUBSUB_EVENT));
		Assert.assertEquals("subscribed", notification.getElement().element("event")
				.element("subscription").attributeValue("subscription"));
		Assert.assertEquals(subscriber, notification.getElement().element("event")
				.element("subscription").attributeValue("jid"));
	}
}