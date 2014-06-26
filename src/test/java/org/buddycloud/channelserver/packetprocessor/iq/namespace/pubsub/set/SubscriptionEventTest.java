package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscriptionMock;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SubscriptionEventTest extends IQTestHandler {
	private IQ request;
	private SubscriptionEvent event;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String subscriber = "francisco@denmark.lit";
	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager dataStore;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		event = new SubscriptionEvent(queue, dataStore);
		request = readStanzaAsIq("/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza");
		event.setServerDomain("shakespeare.lit");

		element = new BaseElement("subscriptions");
		element.addAttribute("node", node);

		dataStore = Mockito.mock(ChannelManager.class);
		Mockito.when(dataStore.isLocalNode(Mockito.anyString())).thenReturn(
				true);
		Mockito.when(dataStore.nodeExists(Mockito.anyString()))
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
		Assert.assertEquals("nodeid-required",
				error.getApplicationConditionName());
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
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
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
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
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
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
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
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void userWithoutSubscriptionReturnsErrorStanza() throws Exception {

		Mockito.when(
				dataStore.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.none,
						Affiliations.none, null));
		event.setChannelManager(dataStore);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized,
				error.getCondition());
	}

	@Test
	public void userWhoIsntOwnerOrModeratorCantUpdateSubscription()
			throws Exception {
		Mockito.when(
				dataStore.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.member, null));

		event.setChannelManager(dataStore);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized,
				error.getCondition());
	}

	@Test
	public void subscribingUserMustHaveExistingSubscriptionToUpdate()
			throws Exception {

		Mockito.when(dataStore.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		NodeMembership membership = new NodeMembershipImpl(node, new JID(
				subscriber), Subscriptions.none, Affiliations.owner, null);
		Mockito.when(
				dataStore.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(membership);

		event.setChannelManager(dataStore);

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.unexpected_request,
				error.getCondition());
	}

	@Test
	public void passingInvalidSubscriptionTypeSetsSubscriptionToNone()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza")
				.replaceFirst("subscription='subscribed'",
						"subscription='i-can-haz-all-the-items'"));

		ArgumentCaptor<NodeSubscription> argument = ArgumentCaptor
				.forClass(NodeSubscription.class);

		Mockito.when(dataStore.nodeExists(node)).thenReturn(true);

		Mockito.when(
				dataStore.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.owner, null));

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers))
				.when(dataStore)
				.getNodeSubscriptionListeners(Mockito.anyString());
		
		event.process(element, jid, request, null);

		Mockito.verify(dataStore, Mockito.times(1)).addUserSubscription(
				argument.capture());
		NodeSubscription subscription = argument.getValue();
		Assert.assertEquals(Subscriptions.none, subscription.getSubscription());
	}

	@Test
	public void passingValidSubscriptionSendsOutExpectedNotifications()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza")
				.replaceFirst("subscription='subscribed'",
						"subscription='subscribed'"));

		Mockito.when(dataStore.nodeExists(node)).thenReturn(true);

		NodeMembership membership = new NodeMembershipImpl(node, new JID(
				subscriber), Subscriptions.subscribed, Affiliations.moderator, null);
		Mockito.when(
				dataStore.getNodeMembership(Mockito.eq(node),
						Mockito.any(JID.class))).thenReturn(membership);

		event.setChannelManager(dataStore);

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionMock(new JID(
				"romeo@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"hamlet@shakespeare.lit")));

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers))
				.when(dataStore)
				.getNodeSubscriptionListeners(Mockito.anyString());

		event.setChannelManager(dataStore);
		event.process(element, jid, request, null);

		// Assert.assertEquals(5, queue.size());
		Packet notification = queue.poll();

		Assert.assertEquals("francisco@denmark.lit/barracks", notification
				.getTo().toString());
		notification = queue.poll();
		Assert.assertEquals("romeo@shakespeare.lit", notification.getTo()
				.toString());
		notification = queue.poll();
		Assert.assertEquals("hamlet@shakespeare.lit", notification.getTo()
				.toString());
		notification = queue.poll();
		Assert.assertEquals("user1@server1", notification.getTo().toString());
		notification = queue.poll();
		Assert.assertEquals("user2@server1", notification.getTo().toString());

		Assert.assertEquals(node, notification.getElement().element("event")
				.element("subscription").attributeValue("node"));
		Assert.assertTrue(notification.toXML().contains(
				JabberPubsub.NS_PUBSUB_EVENT));
		Assert.assertEquals(
				"subscribed",
				notification.getElement().element("event")
						.element("subscription").attributeValue("subscription"));
		Assert.assertEquals(
				subscriber,
				notification.getElement().element("event")
						.element("subscription").attributeValue("jid"));
	}
}