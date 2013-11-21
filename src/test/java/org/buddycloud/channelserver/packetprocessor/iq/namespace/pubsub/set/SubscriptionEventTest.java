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
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
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
import org.xmpp.packet.Roster.Subscription;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SubscriptionEventTest extends IQTestHandler {
	private IQ request;
	private IQ invitationRequest;
	private SubscriptionEvent event;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String subscriber = "francisco@denmark.lit";
	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		event = new SubscriptionEvent(queue, channelManager);
		request = readStanzaAsIq("/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza");
		invitationRequest = readStanzaAsIq("/iq/pubsub/subscription/invitation.stanza");
		event.setServerDomain("shakespeare.lit");

		element = new BaseElement("subscriptions");
		element.addAttribute("node", node);

		channelManager = Mockito.mock(ChannelManager.class);

		event.setChannelManager(channelManager);

		NodeSubscription subscription = new NodeSubscriptionImpl(node, jid,
				Subscriptions.subscribed);

		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						(JID) Mockito.argThat(getBareJidMatcher(new JID(
								"juliet@shakespeare.lit/barracks")))))
				.thenReturn(subscription);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);
	}

	private void setUpListeners() throws NodeStoreException {
		NodeAffiliation subscriptionMockActor = Mockito
				.mock(NodeAffiliation.class);
		Mockito.when(subscriptionMockActor.getAffiliation()).thenReturn(
				Affiliations.owner);

		NodeSubscription subscriptionMockSubscriber = Mockito
				.mock(NodeSubscription.class);
		Mockito.when(subscriptionMockSubscriber.getSubscription()).thenReturn(
				Subscriptions.subscribed);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				subscriptionMockActor);

		Mockito.when(
				channelManager.getUserSubscription(node, new JID(subscriber)))
				.thenReturn(subscriptionMockSubscriber);

		event.setChannelManager(channelManager);

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionMock(new JID(
				"romeo@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"hamlet@shakespeare.lit")));

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers))
				.when(channelManager)
				.getNodeSubscriptionListeners(Mockito.anyString());
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

		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza");
		request.getChildElement().element("subscriptions")
				.element("subscription").detach();

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
		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza");
		request.getChildElement().element("subscriptions")
				.element("subscription").attribute("jid").detach();

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
		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza");
		request.getChildElement().element("subscriptions")
				.element("subscription").attribute("subscription").detach();

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
		Mockito.when(channelManager.nodeExists(Mockito.anyString())).thenThrow(
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

		Mockito.when(channelManager.nodeExists(node)).thenReturn(false);
		event.setChannelManager(channelManager);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void testUserWithoutSubscriptionReturnsErrorStanza()
			throws Exception {

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				null);
		event.setChannelManager(channelManager);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized,
				error.getCondition());
	}

	@Test
	public void testUserWhoIsntOwnerOrModeratorCantUpdateSubscription()
			throws Exception {
		NodeAffiliation subscriptionMock = Mockito.mock(NodeAffiliation.class);
		Mockito.when(subscriptionMock.getAffiliation()).thenReturn(
				Affiliations.member);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				subscriptionMock);
		event.setChannelManager(channelManager);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized,
				error.getCondition());
	}

	@Test
	public void testSubscribingUserMustHaveExistingSubscriptionToUpdate()
			throws Exception {
		NodeAffiliation subscriptionMockActor = Mockito
				.mock(NodeAffiliation.class);
		Mockito.when(subscriptionMockActor.getAffiliation()).thenReturn(
				Affiliations.owner);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				subscriptionMockActor);
		Mockito.when(
				channelManager.getUserSubscription(node, new JID(subscriber)))
				.thenReturn(null);
		event.setChannelManager(channelManager);

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

		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/authorizationPendingGrantReply.stanza");
		request.getChildElement().element("subscriptions")
				.element("subscription").attribute("jid")
				.setValue("i-can-haz-all-the-items");

		NodeAffiliation subscriptionMockActor = Mockito
				.mock(NodeAffiliation.class);
		Mockito.when(subscriptionMockActor.getAffiliation()).thenReturn(
				Affiliations.owner);

		NodeSubscription subscriptionMockSubscriber = Mockito
				.mock(NodeSubscription.class);
		Mockito.when(subscriptionMockSubscriber.getSubscription()).thenReturn(
				Subscriptions.subscribed);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				subscriptionMockActor);
		Mockito.doCallRealMethod().when(channelManager)
				.addUserSubscription(Mockito.any(NodeSubscription.class));
		Mockito.when(
				channelManager.getUserSubscription(node, new JID(subscriber)))
				.thenReturn(subscriptionMockSubscriber);

		event.setChannelManager(channelManager);
		event.process(element, jid, request, null);

		NodeSubscription subscriptionMock = new NodeSubscriptionImpl(node,
				new JID("francisco@denmark.lit"), new JID(
						"francisco@denmark.lit"), Subscriptions.none);

		/*
		 * subscriptionMock Mockito.anyString(), Mockito.any(JID.class),
		 * Mockito.any(JID.class), Mockito.eq(Subscriptions.none));
		 */
		Mockito.verify(channelManager).addUserSubscription(subscriptionMock);
	}

	@Test
	public void testPassingValidSubscriptionSendsOutExpectedNotifications()
			throws Exception {

		setUpListeners();

		event.setChannelManager(channelManager);
		event.process(element, jid, request, null);

		Assert.assertEquals(5, queue.size());
		Packet notification = queue.poll();
		Assert.assertEquals("francisco@denmark.lit/barracks", notification
				.getTo().toString());
		notification = queue.poll();
		Assert.assertEquals("romeo@shakespeare.lit", notification.getTo()
				.toString());
		notification = queue.poll();
		Assert.assertEquals("hamlet@shakespeare.lit", notification.getTo()
				.toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("user1@server1", notification.getTo().toBareJID());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("user2@server1", notification.getTo().toBareJID());

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

	@Test
	public void testUserCanNotInviteAnotherUserIfTheyArentSubscribed()
			throws Exception {
		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						(JID) Mockito.argThat(getBareJidMatcher(new JID(
								"juliet@shakespeare.lit/barracks")))))
				.thenReturn(null);
		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						(JID) Mockito.argThat(getBareJidMatcher(new JID(
								"francisco@denmark.lit"))))).thenReturn(null);
		Mockito.when(
				channelManager.getUserAffiliation(Mockito.anyString(),
						(JID) Mockito.argThat(getBareJidMatcher(new JID(
								"francisco@denmark.lit"))))).thenReturn(null);

		event.process(element, jid, invitationRequest, null);

		IQ response = (IQ) queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized,
				error.getCondition());
	}

	@Test
	public void testUserCanNotInviteAnotherUserIfTheyDontHaveValidSubscription()
			throws Exception {
		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.eq(new JID("francisco@denmark.lit"))))
				.thenReturn(null);

		NodeSubscription subscription = new NodeSubscriptionImpl(node, jid,
				Subscriptions.none);
		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.eq(new JID("hamlet@shakespeare.lit/barracks"))))
				.thenReturn(subscription);

		NodeAffiliation affiliation = new NodeAffiliationImpl(node, jid,
				Affiliations.moderator, new Date());
		Mockito.when(
				channelManager.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(null);

		event.process(element, jid, invitationRequest, null);

		IQ response = (IQ) queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized,
				error.getCondition());
	}

	@Test
	public void testSubscribedUserCanInviteAnotherUser() throws Exception {

		setUpListeners();

		JID invited = new JID("francisco@denmark.lit");

		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.eq(invited))).thenReturn(null);

		NodeSubscription subscription = new NodeSubscriptionImpl(node, jid,
				Subscriptions.subscribed);
		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.eq(jid))).thenReturn(subscription);

		NodeAffiliation affiliation = new NodeAffiliationImpl(node, jid,
				Affiliations.moderator, new Date());
		Mockito.when(
				channelManager.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliation);

		event.process(element, jid, invitationRequest, null);

		IQ response = (IQ) queue.poll();

		Assert.assertNull(response.getError());

		Assert.assertEquals(IQ.Type.result, response.getType());

		Mockito.verify(channelManager, Mockito.times(1)).addUserSubscription(
				(NodeSubscription) Mockito.argThat(getNodeSubscriptionMatcher(
						node, Subscriptions.invited, invited, invited, null,
						jid)));
	}

	@Test
	public void testAlreadyInvitedUserReturnsErrorToSender() throws Exception {

		setUpListeners();

		JID invited = new JID("francisco@denmark.lit");
		NodeSubscription invitedSubscription = new NodeSubscriptionImpl(node,
				invited, Subscriptions.invited);

		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.eq(invited))).thenReturn(invitedSubscription);

		NodeSubscription subscription = new NodeSubscriptionImpl(node, jid,
				Subscriptions.subscribed);
		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.eq(jid))).thenReturn(subscription);

		NodeAffiliation affiliation = new NodeAffiliationImpl(node, jid,
				Affiliations.moderator, new Date());
		Mockito.when(
				channelManager.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliation);

		event.process(element, jid, invitationRequest, null);

		IQ response = (IQ) queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(IQ.Type.error, response.getType());

		Assert.assertEquals(PacketError.Condition.conflict,
				error.getCondition());
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(event.INVITE_IN_PROGRESS,
				error.getApplicationConditionName());
		Assert.assertEquals(JabberPubsub.NS_BUDDYCLOUD_ERROR,
				error.getApplicationConditionNamespaceURI());
	}

	@Test
	public void testInviteSendsOutExpectedNotifications() throws Exception {

		setUpListeners();

		JID invited = new JID("francisco@denmark.lit");

		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.eq(invited))).thenReturn(null);

		NodeSubscription subscription = new NodeSubscriptionImpl(node, jid,
				Subscriptions.subscribed);
		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.eq(jid))).thenReturn(subscription);

		NodeAffiliation affiliation = new NodeAffiliationImpl(node, jid,
				Affiliations.moderator, new Date());
		Mockito.when(
				channelManager.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliation);

		event.process(element, jid, invitationRequest, null);

		Assert.assertEquals(2, queue.size());
		
		IQ response = (IQ) queue.poll();
		
		Assert.assertNull(response.getError());
		Assert.assertEquals(IQ.Type.result, response.getType());
		

		Assert.assertEquals(PacketError.Condition.conflict,
				error.getCondition());
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(event.INVITE_IN_PROGRESS,
				error.getApplicationConditionName());
		Assert.assertEquals(JabberPubsub.NS_BUDDYCLOUD_ERROR,
				error.getApplicationConditionNamespaceURI());
	}
}