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
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class AffiliationEventTest extends IQTestHandler {

	private IQ request;
	private AffiliationEvent event;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String subscriber = "francisco@denmark.lit";
	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");

	private Mock channelManagerMock;

	@Before
	public void setUp() throws Exception {

		channelManagerMock = Mockito.mock(Mock.class);
		Mockito.when(channelManagerMock.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		
		queue = new LinkedBlockingQueue<Packet>();
		event = new AffiliationEvent(queue, channelManagerMock);
		request = readStanzaAsIq("/iq/pubsub/affiliation/affiliationChange.stanza");
		event.setServerDomain("shakespeare.lit");

		element = new BaseElement("affiliations");
		element.addAttribute("node", node);
	}

	@Test
	public void testPassingAffiliationsAsElementNameReturnsTrue() {
		Element element = new BaseElement("affiliations");
		Assert.assertTrue(event.accept(element));
	}

	@Test
	public void testPassingNotAffiliationsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-affiliations");
		Assert.assertFalse(event.accept(element));
	}

	@Test
	public void testNotProvidingNodeAttributeReturnsErrorStanza()
			throws Exception {
		BaseElement element = new BaseElement("affiliations");
		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required", error.getApplicationConditionName());
	}

	@Test
	public void testNotProvidingAffiliationChildNodeReturnsErrorStanza()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst(
						"<affiliation jid='francisco@denmark.lit' affiliation='member'/>",
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
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("jid='francisco@denmark.lit'", ""));
		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}

	@Test
	public void testNotProvidingAffiliationAttributeReturnsErrorStanza()
			throws Exception {
		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", ""));
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

		Mockito.when(channelManagerMock.nodeExists(node)).thenThrow(
				NodeStoreException.class);
		event.setChannelManager(channelManagerMock);

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

		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(false);
		event.setChannelManager(channelManagerMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}

	@Test
	public void testUserWithoutAffiliationReturnsErrorStanza() throws Exception {

		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManagerMock.getUserSubscription(node, jid))
				.thenReturn(null);
		event.setChannelManager(channelManagerMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized, error.getCondition());
	}

	@Test
	public void testUserWhoIsntOwnerOrModeratorCantUpdateAffiliation()
			throws Exception {
		NodeAffiliation affiliationMock = Mockito.mock(NodeAffiliation.class);
		Mockito.when(affiliationMock.getAffiliation()).thenReturn(
				Affiliations.member);

		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManagerMock.getUserAffiliation(node, jid))
				.thenReturn(affiliationMock);
		event.setChannelManager(channelManagerMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized, error.getCondition());
	}

	@Test
	public void testUserMustHaveExistingAffiliationToUpdate() throws Exception {
		NodeAffiliation affiliationMock = Mockito.mock(NodeAffiliation.class);
		Mockito.when(affiliationMock.getAffiliation()).thenReturn(
				Affiliations.owner);

		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		// actorHasPermission
		Mockito.when(channelManagerMock.getUserAffiliation(node, jid))
				.thenReturn(affiliationMock);
		// subscriberHasCurrentAffiliation
		Mockito.when(
				channelManagerMock.getUserAffiliation(node, new JID(
						"francisco@denmark.lit"))).thenReturn(null);
		event.setChannelManager(channelManagerMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.unexpected_request,
				error.getCondition());
	}

	@Test
	public void testItIsNotPossibleToChangeTheAffiliationOfNodeOwner()
			throws Exception {

		NodeAffiliation affiliationSubscriber = Mockito
				.mock(NodeAffiliation.class);
		Mockito.when(affiliationSubscriber.getAffiliation()).thenReturn(
				Affiliations.owner);

		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);

		Mockito.when(
				channelManagerMock.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				affiliationSubscriber);
		event.setChannelManager(channelManagerMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.not_acceptable, error.getCondition());

	}

	@Test
	public void testPassingInvalidAffiliationTypeSetsAffiliationToNone()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'",
						"affiliation='i-can-haz-all-the-items'"));

		NodeAffiliation affiliationActor = Mockito.mock(NodeAffiliation.class);
		Mockito.when(affiliationActor.getAffiliation()).thenReturn(
				Affiliations.moderator);


		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				channelManagerMock.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliationActor);

		event.setChannelManager(channelManagerMock);
		event.process(element, jid, request, null);

		Mockito.verify(channelManagerMock).setUserAffiliation(
				Mockito.anyString(), Mockito.any(JID.class),
				Mockito.eq(Affiliations.none));
	}

	@Test
	public void testPassingValidAffiliationTypeUpdatesAffiliation()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", "affiliation='moderator'"));

		NodeAffiliation affiliationMock = Mockito.mock(NodeAffiliation.class);
		Mockito.when(affiliationMock.getAffiliation()).thenReturn(
				Affiliations.moderator);


		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				channelManagerMock.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliationMock);

		event.setChannelManager(channelManagerMock);
		event.process(element, jid, request, null);

		Mockito.verify(channelManagerMock).setUserAffiliation(
				Mockito.anyString(), Mockito.any(JID.class),
				Mockito.eq(Affiliations.moderator));
	}

	@Test
	public void testPassingValidAffiliationSendsOutExpectedNotifications()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", "affiliation='moderator'"));

		NodeAffiliation affiliationMock = Mockito.mock(NodeAffiliation.class);
		Mockito.when(affiliationMock.getAffiliation()).thenReturn(
				Affiliations.moderator);

		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				channelManagerMock.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliationMock);

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionMock(new JID(
				"romeo@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"hamlet@shakespeare.lit")));

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers)).when(channelManagerMock)
				.getNodeSubscriptionListeners(Mockito.anyString());

		event.setChannelManager(channelManagerMock);
		event.process(element, jid, request, null);

		Assert.assertEquals(2, queue.size());
		Packet notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("romeo@shakespeare.lit", notification.getTo().toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("hamlet@shakespeare.lit", notification.getTo().toString());

		Assert.assertEquals(
				node,
				notification.getElement().element("event")
						.element("affiliation").attributeValue("node"));
		Assert.assertTrue(notification.toXML().contains(JabberPubsub.NS_PUBSUB_EVENT));
		Assert.assertEquals(Affiliations.moderator.toString(), notification
				.getElement().element("event").element("affiliation")
				.attributeValue("affiliation"));
		Assert.assertEquals(subscriber, notification.getElement().element("event")
				.element("affiliation").attributeValue("jid"));
	}
}