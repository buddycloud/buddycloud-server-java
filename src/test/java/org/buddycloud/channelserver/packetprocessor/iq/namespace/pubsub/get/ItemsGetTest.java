package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Arrays;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.jedis.NodeEntryImpl;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class ItemsGetTest extends IQTestHandler {

	private IQ request;
	private ItemsGet itemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private Mock dataStore = new Mock();

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		itemsGet = new ItemsGet(queue, dataStore);
		request = readStanzaAsIq("/iq/pubsub/affiliation/affiliationChange.stanza");
		element = new BaseElement("items");
	}

	@Test
	public void testPassingAffiliationsAsElementNameReturnsTrue() {
		assertTrue(itemsGet.accept(element));
	}

	@Test
	public void testPassingNotCreateAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		assertFalse(itemsGet.accept(element));
	}

	@Test
	public void testMissingNodeAttributeReturnsErrorStanza() throws Exception {
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}

	@Test
	public void testNodeWhichDoesntExistReturnsNotFoundStanza()
			throws Exception {

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(false);
		element.addAttribute("node", node);
		itemsGet.setDataStore(dataStoreMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.cancel, error.getType());
		assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}

	@Test
	public void testDataStoreExceptionReturnsInternalServerErrorStanza()
			throws Exception {
		element.addAttribute("node", node);
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenThrow(
				DataStoreException.class);
		itemsGet.setDataStore(dataStoreMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.wait, error.getType());
		assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testSubscriptionIncompatibleWithItemRetrievalReturnsExpectedStanza()
			throws Exception {

		String affiliation = Affiliations.none.toString();
		String subscription = Subscriptions.none.toString();
		String accessModel = AccessModels.authorize.toString();

		element.addAttribute("node", node);

		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(),
						accessModel.toString())).thenReturn(null);
		itemsGet.setDataStore(dataStoreMock);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.when(
				nodeViewAclMock.canViewNode(node, affiliation, subscription,
						accessModel)).thenReturn(false);
		NodeAclRefuseReason refusalReason = new NodeAclRefuseReason(
				PacketError.Type.auth, PacketError.Condition.forbidden,
				"pending-subscription");
		Mockito.when(nodeViewAclMock.getReason()).thenReturn(refusalReason);
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.auth, error.getType());
		assertEquals(PacketError.Condition.forbidden, error.getCondition());
		assertEquals("pending-subscription",
				error.getApplicationConditionName());
	}

	@Test
	public void testStandardNodeWithNoItemsReturnsNoItems() throws Exception {

		String accessModel = AccessModels.authorize.toString();

		element.addAttribute("node", node);

		DataStore dataStoreMock = Mockito.mock(Mock.class);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscription.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(),
						accessModel.toString())).thenReturn(subscription);

		Mockito.when(
				dataStoreMock.getNodeEntries(Mockito.anyString(),
						Mockito.anyInt(), Mockito.anyString()))
				.thenReturn(null);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);

		itemsGet.setDataStore(dataStoreMock);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.when(
				nodeViewAclMock.canViewNode(node, Affiliations.none.toString(),
						Subscriptions.none.toString(), accessModel))
				.thenReturn(true);
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		assertEquals(IQ.Type.result.toString(), element.attributeValue("type"));
		assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		assertNull(element.element("pubsub").element("items").element("item"));
	}

	@Test
	public void testSubscriptionsNodeWithNoItemsReturnsNoItems()
			throws Exception {
		String accessModel = AccessModels.authorize.toString();
		node = node.replace("posts", "subscriptions");
		element.addAttribute("node", node);

		DataStore dataStoreMock = Mockito.mock(Mock.class);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscription.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(),
						accessModel.toString())).thenReturn(subscription);

		Mockito.when(
				dataStoreMock.getNodeEntries(Mockito.anyString(),
						Mockito.anyInt(), Mockito.anyString()))
				.thenReturn(null);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);

		itemsGet.setDataStore(dataStoreMock);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.when(
				nodeViewAclMock.canViewNode(node, Affiliations.none.toString(),
						Subscriptions.none.toString(), accessModel))
				.thenReturn(true);
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		assertEquals(IQ.Type.result.toString(), element.attributeValue("type"));
		assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		assertNull(element.element("pubsub").element("items").element("item"));
	}

	@Test
	public void testUnparsableNodeEntryIsIgnoredInItemsResponse()
			throws Exception {
		String accessModel = AccessModels.authorize.toString();

		element.addAttribute("node", node);

		DataStore dataStoreMock = Mockito.mock(Mock.class);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscription.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(),
						accessModel.toString())).thenReturn(subscription);

		NodeEntryImpl item = Mockito.mock(NodeEntryImpl.class);
		Mockito.when(item.getId()).thenReturn("id");
		Mockito.when(item.getNode()).thenReturn(node);
		Mockito.when(item.getEntry()).thenReturn("<entry>entry <text><entry>");
		NodeEntryImpl[] items = new NodeEntryImpl[2];
		items[0] = item;
		items[1] = item;
		Iterator<NodeEntryImpl> itemList = Arrays.asList(items).iterator();
		Mockito.doReturn(itemList)
				.when(dataStoreMock)
				.getNodeEntries(Mockito.anyString(), Mockito.anyInt(),
						Mockito.anyString());

		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);

		itemsGet.setDataStore(dataStoreMock);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.when(
				nodeViewAclMock.canViewNode(node, Affiliations.none.toString(),
						Subscriptions.none.toString(), accessModel))
				.thenReturn(true);
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		assertEquals(IQ.Type.result.toString(), element.attributeValue("type"));
		assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		assertEquals(0, element.element("pubsub").element("items").nodeCount());
	}

	@Test
	public void testPostsNodeReturnsItemsAsExpected() throws Exception {
		String accessModel = AccessModels.authorize.toString();

		element.addAttribute("node", node);

		DataStore dataStoreMock = Mockito.mock(Mock.class);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscription.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(),
						accessModel.toString())).thenReturn(subscription);

		NodeEntryImpl item = Mockito.mock(NodeEntryImpl.class);
		Mockito.when(item.getId()).thenReturn("id");
		Mockito.when(item.getNode()).thenReturn(node);
		Mockito.when(item.getEntry()).thenReturn("<entry>entry text</entry>");
		NodeEntryImpl[] items = new NodeEntryImpl[2];
		items[0] = item;
		items[1] = item;
		Iterator<NodeEntryImpl> itemList = Arrays.asList(items).iterator();
		Mockito.doReturn(itemList)
				.when(dataStoreMock)
				.getNodeEntries(Mockito.anyString(), Mockito.anyInt(),
						Mockito.anyString());

		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);

		itemsGet.setDataStore(dataStoreMock);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.when(
				nodeViewAclMock.canViewNode(node, Affiliations.none.toString(),
						Subscriptions.none.toString(), accessModel))
				.thenReturn(true);
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		assertEquals(IQ.Type.result.toString(), element.attributeValue("type"));
		assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));

		assertEquals(2, element.element("pubsub").element("items").nodeCount());
		assertEquals("id",
				element.element("pubsub").element("items").element("item")
						.attributeValue("id"));
		assertEquals("entry text", element.element("pubsub").element("items")
				.element("item").elementText("entry"));
	}

	@Test
	public void testSubscriberThatHasNoSubscribersDoesNotCauseError()
			throws Exception {
		String accessModel = AccessModels.authorize.toString();
		node = node.replace("posts", "subscriptions");

		element.addAttribute("node", node);

		DataStore dataStoreMock = Mockito.mock(Mock.class);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscription.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(),
						accessModel.toString())).thenReturn(subscription);

		NodeSubscriptionImpl item = Mockito.mock(NodeSubscriptionImpl.class);
		Mockito.when(item.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(item.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(item.getBareJID()).thenReturn(jid.toBareJID());
		Mockito.when(item.getNode()).thenReturn(node);
		Mockito.when(item.getForeignChannelServer()).thenReturn(null);
		NodeSubscriptionImpl[] items = new NodeSubscriptionImpl[3];
		items[0] = item;
		items[1] = item;
		items[2] = item;
		Iterator<NodeSubscriptionImpl> itemList = Arrays.asList(items)
				.iterator();
		Mockito.doReturn(itemList).when(dataStoreMock).getNodeSubscribers(node);

		Mockito.doReturn(null)
				.when(dataStoreMock)
				.findUserSubscriptionOfNodes(Mockito.anyString(),
						Mockito.anyString());

		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);

		itemsGet.setDataStore(dataStoreMock);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.when(
				nodeViewAclMock.canViewNode(node, Affiliations.none.toString(),
						Subscriptions.none.toString(), accessModel))
				.thenReturn(true);
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		assertEquals(IQ.Type.result.toString(), element.attributeValue("type"));
		assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));

		assertEquals(3, element.element("pubsub").element("items").nodeCount());
		assertEquals(0,
				element.element("pubsub").element("items").element("item")
						.element("query").elements().size());
	}

	@Test
	public void testSubscriptionsNodeReturnsItemsAsExpected() throws Exception {
		String accessModel = AccessModels.authorize.toString();
		node = node.replace("posts", "subscriptions");

		element.addAttribute("node", node);

		DataStore dataStoreMock = Mockito.mock(Mock.class);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(subscription.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(
				dataStoreMock.getUserSubscriptionOfNode(jid.toBareJID(),
						accessModel.toString())).thenReturn(subscription);

		NodeSubscriptionImpl item = Mockito.mock(NodeSubscriptionImpl.class);
		Mockito.when(item.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(item.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(item.getBareJID()).thenReturn(jid.toBareJID());
		Mockito.when(item.getNode()).thenReturn(node);
		Mockito.when(item.getForeignChannelServer()).thenReturn(null);
		NodeSubscriptionImpl[] items = new NodeSubscriptionImpl[1];
		items[0] = item;
		Iterator<NodeSubscriptionImpl> itemList = Arrays.asList(items)
				.iterator();
		Mockito.doReturn(itemList).when(dataStoreMock).getNodeSubscribers(node);

		NodeSubscriptionImpl childItem = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(childItem.getAffiliation()).thenReturn(
				Affiliations.member.toString());
		Mockito.when(childItem.getSubscription()).thenReturn(
				Subscriptions.subscribed.toString());
		Mockito.when(childItem.getBareJID()).thenReturn(jid.toBareJID());
		Mockito.when(childItem.getNode()).thenReturn(node);
		Mockito.when(childItem.getForeignChannelServer()).thenReturn(null);
		NodeSubscriptionImpl[] childItems = new NodeSubscriptionImpl[1];
		childItems[0] = childItem;
		Iterator<NodeSubscriptionImpl> childItemList = Arrays
				.asList(childItems).iterator();
		Mockito.doReturn(childItemList)
				.when(dataStoreMock)
				.findUserSubscriptionOfNodes(Mockito.anyString(),
						Mockito.anyString());

		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(true);

		itemsGet.setDataStore(dataStoreMock);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.when(
				nodeViewAclMock.canViewNode(node, Affiliations.none.toString(),
						Subscriptions.none.toString(), accessModel))
				.thenReturn(true);
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();
		System.out.println(response.toXML());

		assertEquals(IQ.Type.result.toString(), element.attributeValue("type"));
		assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		assertEquals(1, element.element("pubsub").element("items").nodeCount());
		assertEquals(2,
				element.element("pubsub").element("items").element("item")
						.element("query").nodeCount());
		assertEquals(jid.toBareJID(), element.element("pubsub")
				.element("items").element("item").attributeValue("id"));
		assertEquals(node,
				element.element("pubsub").element("items").element("item")
						.element("query").element("item")
						.attributeValue("node"));
		assertEquals(
				Affiliations.member.toString(),
				element.element("pubsub").element("items").element("item")
						.element("query").element("item")
						.attributeValue("ns1:affiliation"));
		assertEquals(
				Subscriptions.subscribed.toString(),
				element.element("pubsub").element("items").element("item")
						.element("query").element("item")
						.attributeValue("ns2:subscription"));
	}
}