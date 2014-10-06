package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import static org.junit.Assert.assertTrue;

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
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscriptionMock;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class AffiliationEventTest extends IQTestHandler {

	private IQ request;
	private AffiliationEvent event;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String subscriber = "francisco@denmark.lit";
	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");

	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		channelManager = Mockito.mock(ChannelManager.class);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.eq(jid))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.owner, null));
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.eq(new JID("francisco@denmark.lit"))))
				.thenReturn(
						new NodeMembershipImpl(node, jid, Subscriptions.none,
								Affiliations.publisher, null));

		queue = new LinkedBlockingQueue<Packet>();
		event = new AffiliationEvent(queue, channelManager);
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
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required",
				error.getApplicationConditionName());
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
				"/iq/pubsub/affiliation/affiliationChange.stanza")
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
	public void testNotProvidingAffiliationAttributeReturnsErrorStanza()
			throws Exception {
		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", ""));
		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testNodeStoreExceptionResultsInInternalServerErrorStanza()
			throws Exception {

		Mockito.when(channelManager.nodeExists(node)).thenThrow(
				NodeStoreException.class);
		event.setChannelManager(channelManager);

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

		Mockito.when(channelManager.nodeExists(node)).thenReturn(false);
		event.setChannelManager(channelManager);

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void userWithoutAffiliationReturnsErrorStanza() throws Exception {

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.none, null));

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized,
				error.getCondition());
	}

	@Test
	public void userWhoIsntOwnerOrModeratorCantUpdateAffiliation()
			throws Exception {

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.eq(jid))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.publisher, null));

		event.setChannelManager(channelManager);

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.not_authorized,
				error.getCondition());
	}

	@Test
	public void userMustHaveExistingAffiliationToUpdate() throws Exception {

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.eq(jid))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.owner, null));
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.eq(new JID("francisco@denmark.lit"))))
				.thenReturn(
						new NodeMembershipImpl(node, jid, Subscriptions.none,
								Affiliations.none, null));

		event.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.unexpected_request,
				error.getCondition());
	}

	@Test
	public void notPossibleToChangeTheAffiliationOfNodeOwner() throws Exception {

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.eq(jid))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.owner, null));
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.eq(new JID("francisco@denmark.lit"))))
				.thenReturn(
						new NodeMembershipImpl(node, jid, Subscriptions.none,
								Affiliations.owner, null));

		event.process(element, jid, request, null);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.not_acceptable,
				error.getCondition());

	}

	@Test
	public void passingInvalidAffiliationTypeSetsAffiliationToNone()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'",
						"affiliation='i-can-haz-all-the-items'"));

		ResultSet<NodeSubscription> subscriptions = new ResultSetImpl(
				new ArrayList<NodeSubscription>());
		Mockito.when(
				channelManager.getNodeSubscriptionListeners(Mockito.anyString()))
				.thenReturn(subscriptions);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		event.setChannelManager(channelManager);
		event.process(element, jid, request, null);

		Mockito.verify(channelManager).setUserAffiliation(Mockito.anyString(),
				Mockito.any(JID.class), Mockito.eq(Affiliations.none));
	}

	@Test
	public void testPassingValidAffiliationTypeUpdatesAffiliation()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", "affiliation='moderator'"));

		Mockito.when(
				channelManager.getNodeSubscriptionListeners(Mockito.anyString()))
				.thenReturn(
						new ResultSetImpl<NodeSubscription>(
								new ArrayList<NodeSubscription>()));

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		event.setChannelManager(channelManager);
		event.process(element, jid, request, null);

		Mockito.verify(channelManager).setUserAffiliation(Mockito.anyString(),
				Mockito.any(JID.class), Mockito.eq(Affiliations.moderator));
	}

	@Test
	public void testPassingValidAffiliationSendsOutExpectedNotifications()
			throws Exception {

		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", "affiliation='moderator'"));

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionMock(new JID(
				"romeo@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"hamlet@shakespeare.lit")));

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers))
				.when(channelManager)
				.getNodeSubscriptionListeners(Mockito.anyString());

		event.setChannelManager(channelManager);
		event.process(element, jid, request, null);

		// One iq result, 2 user notifications and 2 admin notifications
		Assert.assertEquals(5, queue.size());

		boolean hasIqResult = false;
		boolean hasNotification1 = false;
		boolean hasNotification2 = false;

		for (int i = 0; i < 3; ++i) {
			Packet packet = queue.poll();

			if (packet.getElement().getName().equals("iq")
					&& packet.getTo().equals(
							new JID("francisco@denmark.lit/barracks"))) {
				hasIqResult = true;
			}

			if (packet.getElement().getName().equals("message")
					&& packet.getTo().equals(new JID("romeo@shakespeare.lit"))) {
				hasNotification1 = true;

				Assert.assertEquals(node, packet.getElement().element("event")
						.element("affiliations").attributeValue("node"));
				Assert.assertTrue(packet.toXML().contains(
						JabberPubsub.NS_PUBSUB_EVENT));
				Assert.assertEquals(Affiliations.moderator.toString(), packet
						.getElement().element("event").element("affiliations")
						.element("affiliation").attributeValue("affiliation"));
				Assert.assertEquals(
						subscriber,
						packet.getElement().element("event")
								.element("affiliations").element("affiliation")
								.attributeValue("jid"));
			}

			if (packet.getElement().getName().equals("message")
					&& packet.getTo().equals(new JID("hamlet@shakespeare.lit"))) {
				hasNotification2 = true;

				Assert.assertEquals(node, packet.getElement().element("event")
						.element("affiliations").attributeValue("node"));
				Assert.assertTrue(packet.toXML().contains(
						JabberPubsub.NS_PUBSUB_EVENT));
				Assert.assertEquals(Affiliations.moderator.toString(), packet
						.getElement().element("event").element("affiliations")
						.element("affiliation").attributeValue("affiliation"));
				Assert.assertEquals(
						subscriber,
						packet.getElement().element("event")
								.element("affiliations").element("affiliation")
								.attributeValue("jid"));
			}
		}

		assertTrue("IQ result not sent", hasIqResult);
		assertTrue("Notification to romeo@shakespeare.lit not sent",
				hasNotification1);
		assertTrue("Notification to hamlet@shakespeare.lit not sent",
				hasNotification2);
	}

	public void canNotUpdateOwnAffiliation() throws Exception {

		event.process(element, jid, request, null);

		IQ response = (IQ) queue.poll();
		Assert.assertEquals(IQ.Type.error, response.getType());
		PacketError error = response.getError();
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.not_allowed,
				error.getCondition());
		Assert.assertEquals(event.CAN_NOT_MODIFY_OWN_AFFILIATION,
				error.getApplicationConditionName());
		Assert.assertEquals(Buddycloud.NS_ERROR,
				error.getApplicationConditionNamespaceURI());
	}
	
	@Test
	public void moderatorsCantCreateOtherModerators() throws Exception {
		IQ request = toIq(readStanzaAsString(
				"/iq/pubsub/affiliation/affiliationChange.stanza")
				.replaceFirst("affiliation='member'", "affiliation='moderator'"));
		
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.eq(jid))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.moderator, null));
		
		event.process(element, jid, request, null);

		IQ response = (IQ) queue.poll();
		Assert.assertEquals(IQ.Type.error, response.getType());
		PacketError error = response.getError();
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}
}