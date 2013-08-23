package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class RecentItemsGetTest extends IQTestHandler {

	private IQ request;
	private RecentItemsGet recentItemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	private String TEST_NODE_1 = "node1";
	private String TEST_NODE_2 = "node2";

	private JID TEST_JID_1 = new JID("user1@server1");
	private JID TEST_JID_2 = new JID("user2@server1");

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		channelManager = Mockito.mock(ChannelManager.class);

		recentItemsGet = new RecentItemsGet(queue, channelManager);

		request = readStanzaAsIq("/iq/pubsub/recent-items/request.stanza");
		element = new BaseElement("recent-items");
	}

	@Test
	public void testPassingRecentItemsAsElementNameReturnsTrue() {
		Assert.assertTrue(recentItemsGet.accept(element));
	}

	@Test
	public void testPassingNotRecentItemsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		Assert.assertFalse(recentItemsGet.accept(element));
	}

	@Test
	public void testMissingMaxAttributeReturnsErrorStanza() throws Exception {

		request.getChildElement().element("recent-items")
				.addAttribute("max", null);

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("max-required", error.getApplicationConditionName());
	}

	@Test
	public void testInvalidMaxAttributesReturnsErrorStanza() throws Exception {

		request.getChildElement().element("recent-items")
				.addAttribute("max", "three");

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("invalid-max-value-provided",
				error.getApplicationConditionName());
	}

	@Test
	public void testMissingSinceAttributeReturnsErrorStanza() throws Exception {

		request.getChildElement().element("recent-items")
				.addAttribute("since", null);

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("since-required",
				error.getApplicationConditionName());
	}

	@Test
	public void testInvalidSinceAttributesReturnsErrorStanza() throws Exception {

		request.getChildElement().element("recent-items")
				.addAttribute("since", "a week ago");

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("invalid-since-value-provided",
				error.getApplicationConditionName());
	}

	@Test
	public void testNodeStoreExceptionGeneratesAnErrorStanza() throws Exception {

		Mockito.when(
				channelManager.getRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyInt(), Mockito.any(GlobalItemID.class),
						Mockito.anyString())).thenThrow(
				new NodeStoreException());

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.wait, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testNoRecentItemsReturnsEmptyStanza() throws Exception {

		Mockito.when(
				channelManager.getRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyInt(), Mockito.any(GlobalItemID.class),
						Mockito.anyString())).thenReturn(
				new ClosableIteratorImpl<NodeItem>(new ArrayList<NodeItem>()
						.iterator()));

		recentItemsGet.process(element, jid, request, null);
		IQ response = (IQ) queue.poll();

		Assert.assertEquals(IQ.Type.result, response.getType());
		Element pubsub = response.getChildElement();
		Assert.assertEquals("pubsub", pubsub.getName());
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI,
				pubsub.getNamespaceURI());
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testOutgoingStanzaFormattedAsExpected() throws Exception {

		NodeItem item1 = new NodeItemImpl(TEST_NODE_1, "1", new Date(),
				"<entry>item1</entry>");
		NodeItem item2 = new NodeItemImpl(TEST_NODE_2, "1", new Date(),
				"<entry>item2</entry>");
		NodeItem item3 = new NodeItemImpl(TEST_NODE_1, "2", new Date(),
				"<entry>item3</entry>");
		NodeItem item4 = new NodeItemImpl(TEST_NODE_1, "3", new Date(),
				"<entry>item4</entry>");

		ArrayList<NodeItem> results = new ArrayList<NodeItem>();
		results.add(item1);
		results.add(item2);
		results.add(item3);
		results.add(item4);

		Mockito.when(
				channelManager.getRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyInt(), Mockito.any(GlobalItemID.class),
						Mockito.anyString())).thenReturn(
				new ClosableIteratorImpl<NodeItem>(results.iterator()));

		recentItemsGet.process(element, jid, request, null);
		IQ response = (IQ) queue.poll();

		Assert.assertEquals(IQ.Type.result, response.getType());
		Element pubsub = response.getChildElement();
		Assert.assertEquals("pubsub", pubsub.getName());
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI,
				pubsub.getNamespaceURI());

		List<Element> items = pubsub.elements("items");
		Assert.assertEquals(3, items.size());

		Assert.assertEquals(TEST_NODE_1, items.get(0).attributeValue("node"));
		Assert.assertEquals(TEST_NODE_2, items.get(1).attributeValue("node"));
		Assert.assertEquals(TEST_NODE_1, items.get(2).attributeValue("node"));

		Assert.assertEquals(1, items.get(0).elements("item").size());
		Assert.assertEquals(2, items.get(2).elements("item").size());
	}

	@Test
	public void testUnparsableItemEntriesAreSimplyIgnored() throws Exception {

		NodeItem item1 = new NodeItemImpl(TEST_NODE_1, "1", new Date(),
				"<entry>item1</entry>");
		NodeItem item2 = new NodeItemImpl(TEST_NODE_1, "2", new Date(),
				"<entry>item2");

		ArrayList<NodeItem> results = new ArrayList<NodeItem>();
		results.add(item1);
		results.add(item2);

		Mockito.when(
				channelManager.getRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyInt(), Mockito.any(GlobalItemID.class),
						Mockito.anyString())).thenReturn(
				new ClosableIteratorImpl<NodeItem>(results.iterator()));

		recentItemsGet.process(element, jid, request, null);
		IQ response = (IQ) queue.poll();
		Assert.assertEquals(1, response.getChildElement().element("items")
				.elements("item").size());
	}

	@SuppressWarnings("unchecked")
	@Test
	@Ignore
	public void testCanControlGatheredEntriesUsingRsm() throws Exception {

		NodeItem item1 = new NodeItemImpl(TEST_NODE_1, "node1:1", new Date(0),
				"<entry>item1</entry>");
		NodeItem item2 = new NodeItemImpl(TEST_NODE_2, "node2:1", new Date(10),
				"<entry>item2</entry>");
		NodeItem item3 = new NodeItemImpl(TEST_NODE_1, "node1:2", new Date(20),
				"<entry>item3</entry>");
		NodeItem item4 = new NodeItemImpl(TEST_NODE_1, "node1:3", new Date(30),
				"<entry>item4</entry>");

		ArrayList<NodeItem> results = new ArrayList<NodeItem>();
		results.add(item1);
		results.add(item2);
		results.add(item3);
		results.add(item4);

		Mockito.when(
				channelManager.getRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyInt(), Mockito.any(GlobalItemID.class),
						Mockito.anyString())).thenReturn(
				new ClosableIteratorImpl<NodeItem>(results.iterator()));
		Mockito.when(
				channelManager.getCountRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyString())).thenReturn(results.size());

		Element rsm = request.getElement().addElement("rsm");
		rsm.addNamespace("", RecentItemsGet.NS_RSM);
		rsm.addElement("max").addText("2");
		rsm.addElement("after").addText("node1:1");

		recentItemsGet.process(element, jid, request, null);
		IQ response = (IQ) queue.poll();

		Assert.assertEquals(IQ.Type.result, response.getType());
		Element pubsub = response.getChildElement();
		Assert.assertEquals("pubsub", pubsub.getName());
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI,
				pubsub.getNamespaceURI());

		List<Element> items = pubsub.elements("items");
		Assert.assertEquals(2, items.size());

		Assert.assertEquals(TEST_NODE_2, items.get(0).attributeValue("node"));
		Assert.assertEquals(TEST_NODE_1, items.get(1).attributeValue("node"));
		Assert.assertEquals(1, items.get(0).elements("item").size());
		Assert.assertEquals(1, items.get(1).elements("item").size());

		Element rsmResult = pubsub.element("set");
		Assert.assertEquals("2", rsmResult.element("count").getText());
		Assert.assertEquals("node2:1", rsmResult.element("first").getText());
		Assert.assertEquals("node1:2", rsmResult.element("last").getText());
	}
	
	@Test
	public void testPagingAfterItem() throws Exception {
		Element rsm = new BaseElement(new QName("set", new Namespace("", "http://jabber.org/protocol/rsm")));
		
		GlobalItemID itemID = new GlobalItemIDImpl(new JID("capulet.lit"), "/user/juliet@capulet.lit/posts", "item1");
		GlobalItemID itemID2 = new GlobalItemIDImpl(new JID("montague.lit"), "/user/romeo@capulet.lit/posts", "item1");
		GlobalItemID itemID3 = new GlobalItemIDImpl(new JID("capulet.lit"), "/user/juliet@capulet.lit/posts", "item2");
			
		rsm.addElement("after").setText(itemID.toString());
		rsm.addElement("max").setText("5");
		
		element.addAttribute("node", node);
		
		Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);
		
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member, new Date()));
		
		ArrayList<NodeItem> results = new ArrayList<NodeItem>() {{
			add(new NodeItemImpl(TEST_NODE_1, "1", new Date(), "payload1"));
			add(new NodeItemImpl(TEST_NODE_2, "1", new Date(), "payload2"));
		}};
		
		Mockito.when(
				channelManager.getRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyInt(), Mockito.any(GlobalItemID.class),
						Mockito.anyString())).thenReturn(
				new ClosableIteratorImpl<NodeItem>(new ArrayList<NodeItem>().iterator()));
		Mockito.when(
				channelManager.getCountRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyString())).thenReturn(0);
		
		recentItemsGet.process(element, jid, request, rsm);
		
		verify(channelManager).getRecentItems(eq(jid), any(Date.class), anyInt(), eq(5), eq(itemID), eq("/posts"));

		Packet p = queue.poll(100, TimeUnit.MILLISECONDS);
		
		// Check the response has a valid rsm element
		Element rsmOut = p.getElement().element("pubsub").element("set");
		
		assertEquals("Unexpected count returned", rsmOut.element("count").getText(), "2");
		assertEquals("Unexpected first returned", rsmOut.element("first").getText(), new GlobalItemIDImpl(new JID(""), nodeID, itemID));
		assertEquals("Unexpected last returned", rsmOut.element("last").getText(), "2");
	}
	
	@Test
	public void testPagingAfterItemWithInvalidAfterId() throws Exception {
		Element rsm = new BaseElement(new QName("set", new Namespace("", "http://jabber.org/protocol/rsm")));
		
		rsm.addElement("after").setText("this is invalid");
		
		element.addAttribute("node", "/user/francisco@denmark.lit/posts");
		
		Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);
		
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member, new Date()));
		
		Mockito.when(
				channelManager.getRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyInt(), Mockito.any(GlobalItemID.class),
						Mockito.anyString())).thenReturn(
				new ClosableIteratorImpl<NodeItem>(new ArrayList<NodeItem>().iterator()));
		Mockito.when(
				channelManager.getCountRecentItems(Mockito.any(JID.class),
						Mockito.any(Date.class), Mockito.anyInt(),
						Mockito.anyString())).thenReturn(0);
		
		recentItemsGet.process(element, jid, request, rsm);
		
		Packet p = queue.poll(100, TimeUnit.MILLISECONDS);
		
		assertEquals("Error expected", "error", p.getElement().attributeValue("type"));
	}
}