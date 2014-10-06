package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
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
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
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

		dataStore = mock(ChannelManager.class);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());
		when(dataStore.nodeExists(anyString())).thenReturn(true);

		NodeMembership membership = new NodeMembershipImpl(node, jid,
				Subscriptions.subscribed, Affiliations.member, null);
		when(dataStore.getNodeMembership(anyString(), any(JID.class)))
				.thenReturn(membership);

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionMock(new JID(
				"romeo@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"hamlet@shakespeare.lit")));

		doReturn(new ResultSetImpl<NodeSubscription>(subscribers)).when(
				dataStore).getNodeSubscriptionListeners(anyString());

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
		Packet response = queue.poll();

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
		Packet response = queue.poll();

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
		Packet response = queue.poll();

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
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testNodeStoreExceptionResultsInInternalServerErrorStanza()
			throws Exception {
		when(dataStore.nodeExists(anyString())).thenThrow(
				NodeStoreException.class);

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.wait, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testNonExistantNodeRetunsErrorStanza() throws Exception {

		when(dataStore.nodeExists(node)).thenReturn(false);

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void userWithoutSubscriptionReturnsErrorStanza() throws Exception {

		when(dataStore.getNodeMembership(anyString(), any(JID.class)))
				.thenReturn(
						new NodeMembershipImpl(node, jid, Subscriptions.none,
								Affiliations.none, null));

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}

	@Test
	public void userWhoIsntOwnerOrModeratorCantUpdateSubscription()
			throws Exception {
		when(dataStore.getNodeMembership(anyString(), any(JID.class)))
				.thenReturn(
						new NodeMembershipImpl(node, jid,
								Subscriptions.subscribed, Affiliations.member,
								null));

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}

	@Test
	public void subscribingUserMustHaveExistingSubscriptionToUpdate()
			throws Exception {

		NodeMembership membership = new NodeMembershipImpl(node, new JID(
				subscriber), Subscriptions.none, Affiliations.owner, null);
		when(dataStore.getNodeMembership(anyString(), any(JID.class)))
				.thenReturn(membership);

		NodeMembership inviteeMemebership = new NodeMembershipImpl(node,
				new JID(subscriber), Subscriptions.none, Affiliations.owner,
				null);
		when(dataStore.getNodeMembership(anyString(), eq(jid))).thenReturn(
				inviteeMemebership);

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

		when(dataStore.getNodeMembership(anyString(), any(JID.class)))
				.thenReturn(
						new NodeMembershipImpl(node, jid,
								Subscriptions.subscribed, Affiliations.owner,
								null));

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();

		doReturn(new ResultSetImpl<NodeSubscription>(subscribers)).when(
				dataStore).getNodeSubscriptionListeners(anyString());

		event.process(element, jid, request, null);

		verify(dataStore, times(1)).addUserSubscription(argument.capture());
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

		NodeMembership membership = new NodeMembershipImpl(node, new JID(
				subscriber), Subscriptions.subscribed, Affiliations.moderator,
				null);
		when(dataStore.getNodeMembership(eq(node), any(JID.class))).thenReturn(
				membership);

		event.process(element, jid, request, null);

		Assert.assertEquals(5, queue.size());
		Packet notification = queue.poll();

		Assert.assertEquals(request.getFrom().toString(), notification.getTo()
				.toString());
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

	@Test
	public void userCanInviteAnotherUserToNode() throws Exception {

		JID invitee = new JID("francisco@denmark.lit");
		NodeMembership membership = new NodeMembershipImpl(node, invitee,
				Subscriptions.none, Affiliations.none, null);
		when(dataStore.getNodeMembership(eq(node), eq(invitee))).thenReturn(
				membership);

		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/invite.stanza");

		event.process(element, jid, request, null);

		IQ result = (IQ) queue.poll();

		Assert.assertEquals(IQ.Type.result, result.getType());

		Assert.assertEquals(5, queue.size());

		Message notification = (Message) queue.poll();

		Element subscription = notification.getElement().element("event")
				.element("subscription");
		Assert.assertEquals(Subscriptions.invited, Subscriptions
				.valueOf(subscription.attributeValue("subscription")));
		Assert.assertEquals(invitee,
				new JID(subscription.attributeValue("jid")));
	}

	@Test
	public void userCanNotInviteAnotherUserIfTheyDontHaveValidSubscription()
			throws Exception {

		NodeMembership membership = new NodeMembershipImpl(node, jid,
				Subscriptions.pending, Affiliations.member, null);
		when(dataStore.getNodeMembership(anyString(), any(JID.class)))
				.thenReturn(membership);

		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/invite.stanza");

		event.process(element, jid, request, null);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals(IQ.Type.error, result.getType());
		Assert.assertEquals(PacketError.Type.auth, result.getError().getType());
		Assert.assertEquals(PacketError.Condition.forbidden, result.getError()
				.getCondition());
	}

	@Test
	public void invitedByIsSetAsActorJid() throws Exception {

		JID invitee = new JID("francisco@denmark.lit");
		NodeMembership membership = new NodeMembershipImpl(node, invitee,
				Subscriptions.none, Affiliations.none, null);
		when(dataStore.getNodeMembership(eq(node), eq(invitee))).thenReturn(
				membership);

		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/invite.stanza");

		event.process(element, jid, request, null);

		ArgumentCaptor<NodeSubscription> subscription = ArgumentCaptor
				.forClass(NodeSubscription.class);

		verify(dataStore, times(1)).addUserSubscription(subscription.capture());

		Assert.assertEquals(request.getFrom().toBareJID(), subscription
				.getValue().getInvitedBy().toString());
		Assert.assertEquals(Subscriptions.invited, subscription.getValue()
				.getSubscription());
		Assert.assertEquals(node, subscription.getValue().getNodeId());

	}

	@Test
	public void standardSubscribeDoesNotSetInvitedBy() throws Exception {

		NodeMembership membership = new NodeMembershipImpl(node, new JID(
				subscriber), Subscriptions.subscribed, Affiliations.moderator,
				null);

		when(dataStore.getNodeMembership(eq(node), any(JID.class))).thenReturn(
				membership);

		event.process(element, jid, request, null);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals(IQ.Type.result, result.getType());

		ArgumentCaptor<NodeSubscription> subscription = ArgumentCaptor
				.forClass(NodeSubscription.class);

		verify(dataStore, times(1)).addUserSubscription(subscription.capture());

		Assert.assertNull(subscription.getValue().getInvitedBy());
		Assert.assertEquals(Subscriptions.subscribed, subscription.getValue()
				.getSubscription());
		Assert.assertEquals(node, subscription.getValue().getNodeId());

	}

	@Test
	public void sendsNotificationToInvitedUserIfTheyAreLocal() throws Exception {

		JID invitee = new JID("francisco@denmark.lit");

		NodeMembership membership = new NodeMembershipImpl(node, invitee,
				Subscriptions.none, Affiliations.none, null);
		when(dataStore.getNodeMembership(eq(node), eq(invitee))).thenReturn(
				membership);

		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/invite.stanza");

		event.process(element, jid, request, null);

		IQ result = (IQ) queue.poll();

		Assert.assertEquals(IQ.Type.result, result.getType());

		Assert.assertEquals(5, queue.size());

		Message notification = (Message) queue.poll();

		Element subscription = notification.getElement().element("event")
				.element("subscription");
		Assert.assertEquals(Subscriptions.invited, Subscriptions
				.valueOf(subscription.attributeValue("subscription")));
		Assert.assertEquals(invitee,
				new JID(subscription.attributeValue("jid")));

		queue.poll();
		queue.poll();
		queue.poll();
		Assert.assertEquals(invitee, queue.poll().getTo());
	}

	@Test
	public void sendsNotificationToInvitedUsersServerIfTheyAreNotLocal()
			throws Exception {

		JID invitee = new JID("francisco@denmark.lit");

		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, "shakespeare.lit");
		
		NodeMembership membership = new NodeMembershipImpl(node, invitee,
				Subscriptions.none, Affiliations.none, null);
		when(dataStore.getNodeMembership(eq(node), eq(invitee))).thenReturn(
				membership);

		IQ request = readStanzaAsIq("/iq/pubsub/subscribe/invite.stanza");
		element.addAttribute("node", "/user/pamela@shakespeare.lit/posts");
		event.process(element, jid, request, null);

		IQ result = (IQ) queue.poll();

		Assert.assertEquals(IQ.Type.result, result.getType());

		Assert.assertEquals(5, queue.size());

		Message notification = (Message) queue.poll();

		Element subscription = notification.getElement().element("event")
				.element("subscription");
		Assert.assertEquals(Subscriptions.invited, Subscriptions
				.valueOf(subscription.attributeValue("subscription")));
		Assert.assertEquals(invitee,
				new JID(subscription.attributeValue("jid")));
		Assert.assertEquals(jid,
				new JID(subscription.attributeValue("invited-by")));

		queue.poll();
		queue.poll();
		queue.poll();
		Assert.assertEquals(invitee.getDomain(), queue.poll().getTo()
				.toString());
	}

	@Test
	public void userCanNotModifyOwnSubscription() throws Exception {

		IQ request = this.request.createCopy();

		NodeMembership membership = new NodeMembershipImpl(node, new JID(
				subscriber), Subscriptions.subscribed, Affiliations.moderator,
				null);
		when(dataStore.getNodeMembership(eq(node), any(JID.class))).thenReturn(
				membership);

		event.process(element, new JID("francisco@denmark.lit"), request, null);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals(IQ.Type.error, result.getType());
		PacketError error = result.getError();
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.not_allowed,
				error.getCondition());
		Assert.assertEquals(SubscriptionEvent.CAN_NOT_MODIFY_OWN_SUBSCRIPTION,
				error.getApplicationConditionName());
		Assert.assertEquals(Buddycloud.NS_ERROR,
				error.getApplicationConditionNamespaceURI());
	}
}