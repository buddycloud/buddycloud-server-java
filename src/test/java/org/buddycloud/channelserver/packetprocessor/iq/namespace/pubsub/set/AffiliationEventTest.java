package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
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
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.AffiliationEvent;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
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

public class AffiliationEventTest extends IQTestHandler {

	private IQ request;
	private AffiliationEvent event;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String subscriber = "francisco@denmark.lit";
	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private Mock dataStore = new Mock();

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		event = new AffiliationEvent(queue, dataStore);
		request = readStanzaAsIq("/iq/pubsub/affiliation/affiliationChange.stanza");
		event.setServerDomain("shakespeare.lit");

		element = new BaseElement("affiliations");
		element.addAttribute("node", node);
	}

	@Test
	public void testPassingAffiliationsAsElementNameReturnsTrue() {
		Element element = new BaseElement("affiliations");
		assertTrue(event.accept(element));
	}

	@Test
	public void testPassingNotAffiliationsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-affiliations");
		assertFalse(event.accept(element));
	}

	@Test
	public void testNotProvidingNodeAttributeReturnsErrorStanza()
			throws Exception {
		BaseElement element = new BaseElement("affiliations");
		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
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
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
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
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
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
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.unexpected_request, error.getCondition());
	}

	@Test
	public void testUserWhoIsntOwnerOrModeratorCantUpdateAffiliation()
			throws Exception {
		NodeSubscriptionImpl subscriptionMock = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMock.getAffiliation()).thenReturn("member");

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
	public void testUserMustHaveExistingAffiliationToUpdate() throws Exception {
		NodeSubscriptionImpl subscriptionMockActor = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockActor.getAffiliation())
				.thenReturn("owner");

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(), node))
				.thenReturn(subscriptionMockActor);
		Mockito.when(dataStoreMock.getUserSubscriptionOfNode(subscriber, node))
				.thenReturn(null);
		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.cancel, error.getType());
		assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}

	@Test
	public void testItIsNotPossibleToChangeTheAffiliationOfNodeOwner()
			throws Exception {
		NodeSubscriptionImpl subscriptionMockActor = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockActor.getAffiliation())
				.thenReturn("owner");

		NodeSubscriptionImpl subscriptionMockSubscriber = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockSubscriber.getAffiliation()).thenReturn(
				Affiliations.owner.toString());
		Mockito.when(subscriptionMockSubscriber.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(), node))
				.thenReturn(subscriptionMockActor);
		Mockito.when(dataStoreMock.getUserSubscriptionOfNode(subscriber, node))
				.thenReturn(subscriptionMockSubscriber);
		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.not_acceptable, error.getCondition());
	}

	@Test
	public void testPassingInvalidAffiliationTypeSetsAffiliationToNone()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'",
						"affiliation='i-can-haz-all-the-items'"));

		NodeSubscriptionImpl subscriptionMockActor = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockActor.getAffiliation())
				.thenReturn("owner");

		NodeSubscriptionImpl subscriptionMockSubscriber = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockSubscriber.getAffiliation()).thenReturn(
				"member");
		Mockito.when(subscriptionMockSubscriber.getSubscription()).thenReturn(
				"subscribed");

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(), node))
				.thenReturn(subscriptionMockActor);

		Mockito.when(dataStoreMock.getUserSubscriptionOfNode(subscriber, node))
				.thenReturn(subscriptionMockSubscriber);

		Mockito.when(dataStoreMock.unsubscribeUserFromNode(subscriber, node))
				.thenReturn(true);

		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);

		Mockito.verify(dataStoreMock).subscribeUserToNode(subscriber, node,
				Affiliations.none.toString(),
				Subscriptions.subscribed.toString(), null);
	}

	@Test
	public void testPassingValidAffiliationTypeUpdatesAffiliation()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", "affiliation='moderator'"));

		NodeSubscriptionImpl subscriptionMockActor = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockActor.getAffiliation())
				.thenReturn("owner");

		NodeSubscriptionImpl subscriptionMockSubscriber = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockSubscriber.getAffiliation()).thenReturn(
				"member");
		Mockito.when(subscriptionMockSubscriber.getSubscription()).thenReturn(
				"subscribed");

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(), node))
				.thenReturn(subscriptionMockActor);

		Mockito.when(dataStoreMock.getUserSubscriptionOfNode(subscriber, node))
				.thenReturn(subscriptionMockSubscriber);

		Mockito.when(dataStoreMock.unsubscribeUserFromNode(subscriber, node))
				.thenReturn(true);

		event.setDataStore(dataStoreMock);

		event.process(element, jid, request, null);

		Mockito.verify(dataStoreMock).subscribeUserToNode(subscriber, node,
				Affiliations.moderator.toString(),
				Subscriptions.subscribed.toString(), null);
	}

	@Test
	public void testPassingValidAffiliationSendsOutExpectedNotifications()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", "affiliation='moderator'"));

		NodeSubscriptionImpl subscriptionMockActor = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockActor.getAffiliation())
				.thenReturn("owner");

		NodeSubscriptionImpl subscriptionMockSubscriber = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscriptionMockSubscriber.getAffiliation()).thenReturn(
				"member");
		Mockito.when(subscriptionMockSubscriber.getSubscription()).thenReturn(
				"subscribed");

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(), node))
				.thenReturn(subscriptionMockActor);

		Mockito.when(dataStoreMock.getUserSubscriptionOfNode(subscriber, node))
				.thenReturn(subscriptionMockSubscriber);

		Mockito.when(dataStoreMock.unsubscribeUserFromNode(subscriber, node))
				.thenReturn(true);

		event.setDataStore(dataStoreMock);

		List<NodeSubscriptionMock> subscribers = new ArrayList<NodeSubscriptionMock>();
		subscribers.add(new NodeSubscriptionMock("romeo@shakespeare.lit"));
		subscribers.add(new NodeSubscriptionMock("hamlet@shakespeare.lit"));

		Mockito.doReturn(
				(Iterator<? extends NodeSubscription>) subscribers.iterator())
				.when(dataStoreMock).getNodeSubscribers(Mockito.anyString());

		event.setDataStore(dataStoreMock);
		event.process(element, jid, request, null);

		assertEquals(2, queue.size());
		Packet notification = queue.poll(100, TimeUnit.MILLISECONDS);
		assertEquals("romeo@shakespeare.lit", notification.getTo().toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		assertEquals("hamlet@shakespeare.lit", notification.getTo().toString());

		assertEquals(
				node,
				notification.getElement().element("event")
						.element("affiliation").attributeValue("node"));
		assertTrue(notification.toXML().contains(JabberPubsub.NS_PUBSUB_EVENT));
		assertEquals(Affiliations.moderator.toString(), notification
				.getElement().element("event").element("affiliation")
				.attributeValue("affiliation"));
		assertEquals(subscriber, notification.getElement().element("event")
				.element("affiliation").attributeValue("jid"));
	}
}