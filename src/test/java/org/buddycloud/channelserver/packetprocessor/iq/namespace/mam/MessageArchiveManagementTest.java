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
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
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

	private String node1 = "node1";
	private String node2 = "node2";

	private JID jid1 = new JID("user@server1.com");
	private JID jid2 = new JID("user@server2.com");

	private SimpleDateFormat sdf;

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

		sdf = new SimpleDateFormat(MessageArchiveManagement.DATE_FORMAT);
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		date1 = sdf.parse("1995-10-26T10:00:00Z");
		date2 = sdf.parse("2015-10-21T16:29:00Z");
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

		Mockito.when(
				channelManager.getAffiliationChanges(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.any(Date.class)))
				.thenReturn(noAffiliations);
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
		Assert.assertEquals(date, sdf.parse(delay.attributeValue("stamp")));
		Assert.assertTrue(outgoingMessage.contains(node));
		Assert.assertTrue(outgoingMessage.contains(affiliation.toString()));
	}
}