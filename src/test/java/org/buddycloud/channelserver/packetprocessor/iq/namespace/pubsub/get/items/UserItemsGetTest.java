package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.UserItemsGet;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class UserItemsGetTest extends IQTestHandler {

	private IQ request;
	private UserItemsGet itemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		itemsGet = new UserItemsGet(queue, channelManager);
		request = readStanzaAsIq("/iq/pubsub/items/request.stanza");
		element = new BaseElement("items");

		channelManager = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);
		itemsGet.setChannelManager(channelManager);
	}

	@Test
	public void testPassingItemsAsElementNameReturnsTrue() {
		Assert.assertTrue(itemsGet.accept(element));
	}

	@Test
	public void testPassingNotItemsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		Assert.assertFalse(itemsGet.accept(element));
	}

	@Test
	public void testExternalNodeReturnsExpectedStanzaEarly() throws Exception {

		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(false);

		element.addAttribute("node", "/user/user@remote-server.com/posts");

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		Element element = response.getElement();

		Assert.assertEquals(IQ.Type.get.toString(),
				element.attributeValue("type"));
		Assert.assertEquals("remote-server.com", response.getTo().getDomain());

		Assert.assertEquals(element.element("pubsub").element("actor")
				.getText(), response.getFrom().toBareJID());
	}

	@Test
	public void testNodeWhichDoesntExistReturnsNotFoundStanza()
			throws Exception {

		Mockito.when(channelManager.nodeExists(node)).thenReturn(false);
		element.addAttribute("node", node);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void testNodeStoreExceptionReturnsInternalServerErrorStanza()
			throws Exception {
		element.addAttribute("node", node);

		Mockito.when(channelManager.nodeExists(node)).thenThrow(
				NodeStoreException.class);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.wait, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testSubscriptionIncompatibleWithItemRetrievalReturnsExpectedStanza()
			throws Exception {

		element.addAttribute("node", node);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				null);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.doReturn(false)
				.when(nodeViewAclMock)
				.canViewNode(Mockito.anyString(),
						Mockito.any(Affiliations.class),
						Mockito.any(Subscriptions.class),
						Mockito.any(AccessModels.class));
		NodeAclRefuseReason refusalReason = new NodeAclRefuseReason(
				PacketError.Type.auth, PacketError.Condition.forbidden,
				"pending-subscription");
		Mockito.when(nodeViewAclMock.getReason()).thenReturn(refusalReason);
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
		Assert.assertEquals("pending-subscription",
				error.getApplicationConditionName());
	}

	@Test
	public void testStandardNodeWithNoItemsReturnsNoItems() throws Exception {

		AccessModels accessModel = AccessModels.authorize;

		element.addAttribute("node", node);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		NodeAffiliationImpl affiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(affiliation.getAffiliation()).thenReturn(
				Affiliations.member);
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				subscription);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				affiliation);

		Mockito.when(
				channelManager.getNodeItems(Mockito.anyString(),
						Mockito.anyString(), Mockito.anyInt()))
				.thenReturn(null);
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.doReturn(true)
				.when(nodeViewAclMock)
				.canViewNode(Mockito.anyString(),
						Mockito.any(Affiliations.class),
						Mockito.any(Subscriptions.class),
						Mockito.any(AccessModels.class));
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		Assert.assertEquals(IQ.Type.result.toString(),
				element.attributeValue("type"));
		Assert.assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		Assert.assertNull(element.element("pubsub").element("items")
				.element("item"));
	}

	@Test
	public void testSubscriptionsNodeWithNoItemsReturnsNoItems()
			throws Exception {

		node = node.replace("posts", "subscriptions");
		element.addAttribute("node", node);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		NodeAffiliationImpl affiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(affiliation.getAffiliation()).thenReturn(
				Affiliations.member);
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				subscription);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				affiliation);

		Mockito.when(
				channelManager.getNodeItems(Mockito.anyString(),
						Mockito.anyString(), Mockito.anyInt()))
				.thenReturn(null);
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.doReturn(true)
				.when(nodeViewAclMock)
				.canViewNode(Mockito.anyString(),
						Mockito.any(Affiliations.class),
						Mockito.any(Subscriptions.class),
						Mockito.any(AccessModels.class));
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		Assert.assertEquals(IQ.Type.result.toString(),
				element.attributeValue("type"));
		Assert.assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		Assert.assertNull(element.element("pubsub").element("items")
				.element("item"));
	}

	@Test
	public void testUnparsableNodeEntryIsIgnoredInItemsResponse()
			throws Exception {
		AccessModels accessModel = AccessModels.authorize;

		element.addAttribute("node", node);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		NodeAffiliationImpl affiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(affiliation.getAffiliation()).thenReturn(
				Affiliations.member);
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				subscription);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				affiliation);

		NodeItem item = Mockito.mock(NodeItem.class);
		Mockito.when(item.getId()).thenReturn("id");
		Mockito.when(item.getNodeId()).thenReturn(node);
		Mockito.when(item.getPayload())
				.thenReturn("<entry>entry <text><entry>");
		NodeItem[] items = new NodeItem[2];
		items[0] = item;
		items[1] = item;
		CloseableIterator<NodeItem> itemList = new ClosableIteratorImpl<NodeItem>(
				Arrays.asList(items).iterator());
		Mockito.doReturn(itemList)
				.when(channelManager)
				.getNodeItems(Mockito.anyString(), Mockito.anyString(),
						Mockito.anyInt());

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.doReturn(true)
				.when(nodeViewAclMock)
				.canViewNode(Mockito.anyString(),
						Mockito.any(Affiliations.class),
						Mockito.any(Subscriptions.class),
						Mockito.any(AccessModels.class));
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		Assert.assertEquals(IQ.Type.result.toString(),
				element.attributeValue("type"));
		Assert.assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		Assert.assertEquals(0, element.element("pubsub").element("items")
				.nodeCount());
	}

	@Test
	public void testPostsNodeReturnsItemsAsExpected() throws Exception {
		AccessModels accessModel = AccessModels.authorize;

		element.addAttribute("node", node);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		NodeAffiliationImpl affiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(affiliation.getAffiliation()).thenReturn(
				Affiliations.member);
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				subscription);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				affiliation);

		NodeItem item = Mockito.mock(NodeItem.class);
		Mockito.when(item.getId()).thenReturn("id");
		Mockito.when(item.getNodeId()).thenReturn(node);
		Mockito.when(item.getPayload()).thenReturn("<entry>entry text</entry>");
		NodeItem[] items = new NodeItem[2];
		items[0] = item;
		items[1] = item;
		CloseableIterator<NodeItem> itemList = new ClosableIteratorImpl<NodeItem>(
				Arrays.asList(items).iterator());
		Mockito.doReturn(itemList)
				.when(channelManager)
				.getNodeItems(Mockito.anyString(), Mockito.anyString(),
						Mockito.anyInt());

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.doReturn(true)
				.when(nodeViewAclMock)
				.canViewNode(Mockito.anyString(),
						Mockito.any(Affiliations.class),
						Mockito.any(Subscriptions.class),
						Mockito.any(AccessModels.class));
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		Assert.assertEquals(IQ.Type.result.toString(),
				element.attributeValue("type"));
		Assert.assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));

		Assert.assertEquals(2, element.element("pubsub").element("items")
				.nodeCount());
		Assert.assertEquals("id", element.element("pubsub").element("items")
				.element("item").attributeValue("id"));
		Assert.assertEquals("entry text",
				element.element("pubsub").element("items").element("item")
						.elementText("entry"));
	}

	@Test
	public void testSubscriberThatHasNoSubscribersDoesNotCauseError()
			throws Exception {
		AccessModels accessModel = AccessModels.authorize;
		node = node.replace("posts", "subscriptions");

		element.addAttribute("node", node);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		NodeAffiliationImpl affiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(affiliation.getAffiliation()).thenReturn(
				Affiliations.member);
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				subscription);
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				affiliation);

		NodeAffiliationImpl itemAffiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(itemAffiliation.getAffiliation()).thenReturn(
				Affiliations.member);

		NodeSubscriptionImpl itemSubscription1 = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(itemSubscription1.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(itemSubscription1.getUser()).thenReturn(jid);
		Mockito.when(itemSubscription1.getNodeId()).thenReturn(node);
		Mockito.when(itemSubscription1.getUID()).thenReturn(jid.toString());

		JID jid2 = new JID("mercutio@shakespeare.lit");
		NodeSubscriptionImpl itemSubscription2 = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(itemSubscription2.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(itemSubscription2.getUser()).thenReturn(jid2);
		Mockito.when(itemSubscription2.getNodeId()).thenReturn(node);
		Mockito.when(itemSubscription2.getUID()).thenReturn(jid2.toString());

		JID jid3 = new JID("titania@shakespeare.lit");
		NodeSubscriptionImpl itemSubscription3 = Mockito
				.mock(NodeSubscriptionImpl.class);
		Mockito.when(itemSubscription3.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(itemSubscription3.getUser()).thenReturn(jid3);
		Mockito.when(itemSubscription3.getNodeId()).thenReturn(node);
		Mockito.when(itemSubscription3.getUID()).thenReturn(jid3.toString());

		ArrayList<NodeSubscription> items = new ArrayList<NodeSubscription>();
		items.add(itemSubscription1);
		items.add(itemSubscription2);
		items.add(itemSubscription3);
		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(items))
				.when(channelManager).getNodeSubscriptions(node);

		Mockito.doReturn(null).when(channelManager)
				.getUserAffiliation(node, new JID("pamela@denmark.lit"));

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.doReturn(true)
				.when(nodeViewAclMock)
				.canViewNode(Mockito.anyString(),
						Mockito.any(Affiliations.class),
						Mockito.any(Subscriptions.class),
						Mockito.any(AccessModels.class));
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		Assert.assertEquals(IQ.Type.result.toString(),
				element.attributeValue("type"));
		Assert.assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));

		Assert.assertEquals(3, element.element("pubsub").element("items")
				.nodeCount());
		Assert.assertEquals(0, element.element("pubsub").element("items")
				.element("item").element("query").elements().size());
	}

	@Test
	public void testSubscriptionsNodeReturnsItemsAsExpected() throws Exception {

		AccessModels accessModel = AccessModels.authorize;
		node = node.replace("posts", "subscriptions");

		element.addAttribute("node", node);

		NodeSubscriptionImpl subscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		NodeAffiliationImpl affiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(affiliation.getAffiliation()).thenReturn(
				Affiliations.member);
		Mockito.when(subscription.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				subscription);
		Mockito.when(
				channelManager.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliation);

		NodeSubscriptionImpl itemSubscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		NodeAffiliationImpl itemAffiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(itemSubscription.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		Mockito.when(itemSubscription.getUser()).thenReturn(jid);
		Mockito.when(itemSubscription.getUID()).thenReturn(jid.toString());
		Mockito.when(itemSubscription.getNodeId()).thenReturn(node);
		ArrayList items = new ArrayList<NodeSubscriptionImpl>();
		items.add(itemSubscription);
		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(items))
				.when(channelManager).getNodeSubscriptions(node);

		NodeSubscriptionImpl childItemSubscription = Mockito
				.mock(NodeSubscriptionImpl.class);
		NodeAffiliationImpl childItemAffiliation = Mockito
				.mock(NodeAffiliationImpl.class);
		Mockito.when(childItemAffiliation.getAffiliation()).thenReturn(
				Affiliations.member);
		Mockito.when(childItemSubscription.getUser()).thenReturn(jid);
		Mockito.when(childItemSubscription.getUID()).thenReturn(jid.toString());
		Mockito.when(childItemSubscription.getNodeId()).thenReturn(
				"/user/juliet@shakespeare.lit/posts");
		Mockito.when(childItemSubscription.getListener()).thenReturn(jid);
		Mockito.when(childItemSubscription.getSubscription()).thenReturn(
				Subscriptions.subscribed);
		ArrayList<NodeSubscription> childItems = new ArrayList<NodeSubscription>();
		childItems.add(childItemSubscription);
		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(childItems))
				.when(channelManager)
				.getUserSubscriptions(new JID("juliet@shakespeare.lit"));

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);

		NodeViewAcl nodeViewAclMock = Mockito.mock(NodeViewAcl.class);
		Mockito.doReturn(true)
				.when(nodeViewAclMock)
				.canViewNode(Mockito.anyString(),
						Mockito.any(Affiliations.class),
						Mockito.any(Subscriptions.class),
						Mockito.any(AccessModels.class));
		itemsGet.setNodeViewAcl(nodeViewAclMock);

		itemsGet.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		Element element = response.getElement();

		System.out.println(response.toXML());
		
		Assert.assertEquals(IQ.Type.result.toString(),
				element.attributeValue("type"));
		Assert.assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		Assert.assertEquals(1, element.element("pubsub").element("items")
				.nodeCount());
		Assert.assertEquals(2, element.element("pubsub").element("items")
				.element("item").element("query").nodeCount());
		Assert.assertEquals(jid.toBareJID(),
				element.element("pubsub").element("items").element("item")
						.attributeValue("id"));
		Assert.assertEquals("/user/juliet@shakespeare.lit/posts",
				element.element("pubsub").element("items").element("item")
						.element("query").element("item")
						.attributeValue("node"));
		Assert.assertEquals(
				Affiliations.member.toString(),
				element.element("pubsub")
						.element("items")
						.element("item")
						.element("query")
						.element("item")
						.attributeValue(
								new QName("affiliation", new Namespace("ns1",
										JabberPubsub.NAMESPACE_URI))));
		Assert.assertEquals(
				Subscriptions.subscribed.toString(),
				element.element("pubsub")
						.element("items")
						.element("item")
						.element("query")
						.element("item")
						.attributeValue(
								new QName("subscription", new Namespace("ns2",
										JabberPubsub.NAMESPACE_URI))));
	}

	@Test
	public void testPagingAfterItem() throws Exception {
		Element rsm = new BaseElement(new QName("set", new Namespace("",
				"http://jabber.org/protocol/rsm")));

		rsm.addElement("after").setText(
				"tag:denmark.lit,/user/" + node + "/posts,item-id");

		element.addAttribute("node", node);

		Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);

		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member,
						new Date()));

		itemsGet.process(element, jid, request, rsm);

		verify(channelManager).getNodeItems(anyString(), eq("item-id"),
				anyInt());
	}

	@Test
	public void testPagingAfterItemWithPlainNodeID() throws Exception {
		Element rsm = new BaseElement(new QName("set", new Namespace("",
				"http://jabber.org/protocol/rsm")));

		rsm.addElement("after").setText("item-id");

		element.addAttribute("node", node);

		Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);

		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member,
						new Date()));

		itemsGet.process(element, jid, request, rsm);

		verify(channelManager).getNodeItems(anyString(), eq("item-id"),
				anyInt());
	}

	@Test
	public void testPagingAfterItemWithInvalidAfterId() throws Exception {
		Element rsm = new BaseElement(new QName("set", new Namespace("",
				"http://jabber.org/protocol/rsm")));

		rsm.addElement("after").setText("tag:this is invalid");

		element.addAttribute("node", "/user/francisco@denmark.lit/posts");

		Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);

		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member,
						new Date()));

		itemsGet.process(element, jid, request, rsm);

		Packet p = queue.poll(100, TimeUnit.MILLISECONDS);

		assertEquals("Error expected", "error",
				p.getElement().attributeValue("type"));
	}

	@Test
	public void testPagingAfterItemWithInvalidNode() throws Exception {
		Element rsm = new BaseElement(new QName("set", new Namespace("",
				"http://jabber.org/protocol/rsm")));

		rsm.addElement("after").setText(
				"tag:denmark.lit,/user/juliet@capulet.lit/posts,item-id");

		element.addAttribute("node", "/user/francisco@denmark.lit/posts");

		Mockito.when(channelManager.nodeExists(anyString())).thenReturn(true);

		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member,
						new Date()));

		itemsGet.process(element, jid, request, rsm);

		Packet p = queue.poll(100, TimeUnit.MILLISECONDS);

		assertEquals("Error expected", "error",
				p.getElement().attributeValue("type"));
	}

	@Test
	public void testGetItemNotFoundIfSingleItemNotFound() throws Exception {
		element.addAttribute("node", "/user/francisco@denmark.lit/posts");
		element.addElement("item").addAttribute("id", "12345");

		Mockito.when(channelManager.getNodeItem(eq(node), Mockito.anyString()))
				.thenReturn(null);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member,
						new Date()));

		itemsGet.process(element, jid, request, null);

		Packet p = queue.poll();
		assertEquals("Error expected", "error",
				p.getElement().attributeValue("type"));
		assertEquals("Expected 'cancel'", "cancel",
				p.getElement().element("error").attributeValue("type"));
		assertNotNull(p.getElement().element("error").element("item-not-found"));
	}

	@Test
	public void testCanRetrieveSingleItem() throws Exception {

		String id = "12345";
		String payload = "<entry>entry text</entry>";

		element.addAttribute("node", node);
		element.addElement("item").addAttribute("id", id);

		NodeItem item = new NodeItemImpl(node, id, new Date(), payload);

		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);
		Mockito.when(
				channelManager.getNodeItem(Mockito.anyString(),
						Mockito.anyString())).thenReturn(item);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member,
						new Date()));

		itemsGet.process(element, jid, request, null);

		Packet response = queue.poll();

		Element element = response.getElement();

		Assert.assertEquals(IQ.Type.result.toString(),
				element.attributeValue("type"));
		Assert.assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));

		Assert.assertEquals(1, element.element("pubsub").element("items")
				.nodeCount());
		Assert.assertEquals(node, element.element("pubsub").element("items")
				.attributeValue("node"));
		Assert.assertEquals(id, element.element("pubsub").element("items")
				.element("item").attributeValue("id"));
		Assert.assertEquals("entry text",
				element.element("pubsub").element("items").element("item")
						.elementText("entry"));
	}
}