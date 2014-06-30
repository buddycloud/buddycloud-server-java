package org.buddycloud.channelserver.packetprocessor.iq.namespace.mam;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.TimeZone;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerImpl;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class MessageArchiveManagementTest extends IQTestHandler {

	private ChannelManager channelManager;
	private MessageArchiveManagement mam;
	private LinkedBlockingQueue<Packet> queue;
	private IQ request;

	private ResultSet<NodeAffiliation> noAffiliations;
	private ResultSet<NodeSubscription> noSubscriptions;
	private CloseableIterator<NodeItem> noItems;

	private Date date1 = new Date();
	private Date date2 = new Date();
	private Date date3 = new Date();

	private String node1 = "node1";
	private String node2 = "node2";

	private JID jid1 = new JID("user@server1.com");
	private JID jid2 = new JID("user@server2.com");
	private JID invitedBy = new JID("romeo@shakespeare.lit");

	@Before
	public void setUp() throws Exception {
		channelManager = Mockito.mock(ChannelManagerImpl.class);
		queue = new LinkedBlockingQueue<Packet>();
		mam = new MessageArchiveManagement(queue, channelManager);

		request = readStanzaAsIq("/iq/mam/request.stanza");

		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);

		noAffiliations = new ResultSetImpl<NodeAffiliation>(
				new ArrayList<NodeAffiliation>());
		noSubscriptions = new ResultSetImpl<NodeSubscription>(
				new ArrayList<NodeSubscription>());
		noItems = new ClosableIteratorImpl<NodeItem>(
				new LinkedList<NodeItem>().iterator());

		date1 = Conf.parseDate("1995-10-26T10:00:00Z");
		date2 = Conf.parseDate("2015-10-21T16:29:00Z");
		date3 = Conf.parseDate("1985-10-27T09:59:00Z");

		Mockito.when(
				channelManager.getAffiliationChanges(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(noAffiliations);
		Mockito.when(
				channelManager.getSubscriptionChanges(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(noSubscriptions);
		Mockito.when(
				channelManager.getNewNodeItemsForUser(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(noItems);

		NodeMembership membership = new NodeMembershipImpl(node1, jid1,
				Subscriptions.subscribed, Affiliations.owner, null);
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(membership);
	}

	@Test
	public void testRequestsOnlyHonouredForLocalUsers() throws Exception {
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(false);

		mam.process(request);

		Assert.assertEquals(1, queue.size());

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.service_unavailable,
				error.getCondition());
	}

	@Test
	public void testInvalidStartTimestampResultsInBadRequestStanza()
			throws Exception {

		request.getChildElement().element("start").setText("not-a-date");
		mam.process(request);

		Assert.assertEquals(1, queue.size());

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testInvalidEndTimestampResultsInBadRequestStanza()
			throws Exception {

		request.getChildElement().element("end").setText("not-a-date");
		mam.process(request);

		Assert.assertEquals(1, queue.size());

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testNoNotificationsResultsInJustResultPacket() throws Exception {

		mam.process(request);

		Assert.assertEquals(1, queue.size());
		IQ result = (IQ) queue.poll();
		Assert.assertEquals("result", result.getType().toString());
	}

	@Test
	public void testTwoAffiliationChangesReportAsExpected() throws Exception {

		ArrayList<NodeAffiliation> affiliations = new ArrayList<NodeAffiliation>();

		affiliations.add(new NodeAffiliationImpl(node1, jid1,
				Affiliations.member, date1));
		affiliations.add(new NodeAffiliationImpl(node2, jid2,
				Affiliations.publisher, date2));

		Mockito.when(
				channelManager.getAffiliationChanges(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(new ResultSetImpl<NodeAffiliation>(affiliations));

		mam.process(request);

		Assert.assertEquals(3, queue.size());
		checkAffiliationStanza(queue.poll(), jid1, date1, node1,
				Affiliations.member);
		checkAffiliationStanza(queue.poll(), jid2, date2, node2,
				Affiliations.publisher);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals("result", result.getType().toString());
	}

	@Test
	public void testOutcastChangeReportedAsExpected() throws Exception {

		NodeMembership membership = new NodeMembershipImpl(node1, jid1,
				Subscriptions.subscribed, Affiliations.owner, null);
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(membership);

		ArrayList<NodeAffiliation> affiliations = new ArrayList<NodeAffiliation>();

		affiliations.add(new NodeAffiliationImpl(node1, jid1,
				Affiliations.outcast, date1));

		Mockito.when(
				channelManager.getAffiliationChanges(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(new ResultSetImpl<NodeAffiliation>(affiliations));

		mam.process(request);

		Assert.assertEquals(2, queue.size());
		checkAffiliationStanza(queue.poll(), jid1, date1, node1,
				Affiliations.none);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals("result", result.getType().toString());
	}

	@Test
	public void testOutcastChangeReportedAsExpectedToOwnerModerator()
			throws Exception {

		ArrayList<NodeAffiliation> affiliations = new ArrayList<NodeAffiliation>();

		affiliations.add(new NodeAffiliationImpl(node1, jid1,
				Affiliations.member, date1));
		affiliations.add(new NodeAffiliationImpl(node2, jid2,
				Affiliations.publisher, date2));

		Mockito.when(
				channelManager.getAffiliationChanges(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(new ResultSetImpl<NodeAffiliation>(affiliations));

		mam.process(request);

		Assert.assertEquals(3, queue.size());
		checkAffiliationStanza(queue.poll(), jid1, date1, node1,
				Affiliations.member);
		checkAffiliationStanza(queue.poll(), jid2, date2, node2,
				Affiliations.publisher);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals("result", result.getType().toString());
	}

	private void checkAffiliationStanza(Packet result, JID jid, Date date,
			String node, Affiliations affiliation) throws ParseException {

		Element message = result.getElement();
		Assert.assertEquals(MessageArchiveManagement.NAMESPACE_MAM, message
				.element("result").getNamespaceURI());
		Assert.assertEquals(MessageArchiveManagement.NAMESPACE_FORWARDED,
				message.element("forwarded").getNamespaceURI());

		Element delay = message.element("forwarded").element("delay");

		// Using strings here as SMACK doesn't output correct elements :-(
		String outgoingMessage = message.asXML();
		Assert.assertTrue(outgoingMessage.contains("event"));
		Assert.assertTrue(outgoingMessage.contains("affiliations"));
		Assert.assertTrue(outgoingMessage.contains(jid.toBareJID()));
		Assert.assertEquals(date, Conf.parseDate(delay.attributeValue("stamp")));
		Assert.assertTrue(outgoingMessage.contains(node));
		Assert.assertTrue(outgoingMessage.contains(affiliation.toString()));
	}

	@Test
	public void testThreeSubscriptionChangesReportAsExpected() throws Exception {

		NodeMembership publisher = new NodeMembershipImpl(node1, jid1,
				Subscriptions.subscribed, Affiliations.publisher, null);
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(publisher);
		
		ArrayList<NodeSubscription> subscriptions = new ArrayList<NodeSubscription>();
		subscriptions.add(new NodeSubscriptionImpl(node1, jid1,
				Subscriptions.subscribed, null, date1));
		subscriptions.add(new NodeSubscriptionImpl(node2, jid2,
				Subscriptions.invited, invitedBy, date2));
		subscriptions.add(new NodeSubscriptionImpl(node2, jid2,
				Subscriptions.pending, invitedBy, date2));

		Mockito.when(
				channelManager.getSubscriptionChanges(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(new ResultSetImpl<NodeSubscription>(subscriptions));

		mam.process(request);

		Assert.assertEquals(3, queue.size());
		checkSubscriptionStanza(queue.poll(), jid1, date1, node1,
				Subscriptions.subscribed, null);
		checkSubscriptionStanza(queue.poll(), jid2, date2, node2,
				Subscriptions.pending, null);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals("result", result.getType().toString());
	}

	@Test
	public void ownerModeratorAreShownInvites() throws Exception {

		NodeMembership owner = new NodeMembershipImpl(node1, jid1,
				Subscriptions.subscribed, Affiliations.owner, null);
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(owner);
		
		JID invitedBy = new JID("romeo@shakespeare.lit");

		ArrayList<NodeSubscription> subscriptions = new ArrayList<NodeSubscription>();
		subscriptions.add(new NodeSubscriptionImpl(node1, jid1,
				Subscriptions.subscribed, null, date1));
		subscriptions.add(new NodeSubscriptionImpl(node2, jid2,
				Subscriptions.invited, invitedBy, date2));
		subscriptions.add(new NodeSubscriptionImpl(node2, jid2,
				Subscriptions.pending, invitedBy, date2));

		Mockito.when(
				channelManager.getSubscriptionChanges(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(new ResultSetImpl<NodeSubscription>(subscriptions));

		mam.process(request);

		Assert.assertEquals(4, queue.size());
		checkSubscriptionStanza(queue.poll(), jid1, date1, node1,
				Subscriptions.subscribed, null);
		checkSubscriptionStanza(queue.poll(), jid2, date2, node2,
				Subscriptions.invited, invitedBy);
		checkSubscriptionStanza(queue.poll(), jid2, date2, node2,
				Subscriptions.pending, invitedBy);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals("result", result.getType().toString());
	}

	private void checkSubscriptionStanza(Packet result, JID jid, Date date,
			String node, Subscriptions subscription, JID invitedBy)
			throws ParseException {
		Element message = result.getElement();
		Assert.assertEquals(MessageArchiveManagement.NAMESPACE_MAM, message
				.element("result").getNamespaceURI());
		Assert.assertEquals(MessageArchiveManagement.NAMESPACE_FORWARDED,
				message.element("forwarded").getNamespaceURI());

		Element delay = message.element("forwarded").element("delay");
		Element sub = message.element("forwarded").element("event")
				.element("subscription");

		Assert.assertEquals(node, sub.attributeValue("node"));
		Assert.assertEquals(jid.toBareJID(), sub.attributeValue("jid"));
		Assert.assertEquals(subscription,
				Subscriptions.valueOf(sub.attributeValue("subscription")));
		if (null == invitedBy) {
			Assert.assertNull(sub.attributeValue("invited-by"));
		} else {
			Assert.assertEquals(invitedBy.toBareJID(),
					sub.attributeValue("invited-by"));
		}
		Assert.assertEquals(date, Conf.parseDate(delay.attributeValue("stamp")));
	}

	@Test
	public void testTwoNewItemsReportAsExpected() throws Exception {

		String item1 = "<entry>item1</entry>";
		String item2 = "<entry>item2</entry>";
		String item3 = "<entry>item3</entry>";

		ArrayList<NodeItem> items = new ArrayList<NodeItem>();
		items.add(new NodeItemImpl(node1, "1", date1, item1));
		items.add(new NodeItemImpl(node1, "2", date2, item2));
		items.add(new NodeItemImpl(node2, "1", date3, item3));

		Mockito.when(
				channelManager.getNewNodeItemsForUser(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(
						new ClosableIteratorImpl<NodeItem>(items.iterator()));

		mam.process(request);

		Assert.assertEquals(4, queue.size());
		checkItemStanza(queue.poll(), date1, node1, "1", item1);
		checkItemStanza(queue.poll(), date2, node1, "2", item2);
		checkItemStanza(queue.poll(), date3, node2, "1", item3);

		IQ result = (IQ) queue.poll();
		Assert.assertEquals("result", result.getType().toString());
	}

	private void checkItemStanza(Packet result, Date date, String node,
			String id, String entry) throws ParseException {

		Element message = result.getElement();
		Assert.assertEquals(MessageArchiveManagement.NAMESPACE_MAM, message
				.element("result").getNamespaceURI());
		Assert.assertEquals(MessageArchiveManagement.NAMESPACE_FORWARDED,
				message.element("forwarded").getNamespaceURI());

		Element delay = message.element("forwarded").element("delay");
		Element items = message.element("forwarded").element("event")
				.element("items");
		Element item = items.element("item");

		Assert.assertEquals(id, item.attributeValue("id"));
		Assert.assertEquals(node, items.attributeValue("node"));
		// Hack to make up for SMACK
		Assert.assertTrue(item.asXML().replace(" xmlns=\"\"", "")
				.contains(entry));

		Assert.assertEquals(date, Conf.parseDate(delay.attributeValue("stamp")));
	}
}