package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
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

public class ItemDeleteTest extends IQTestHandler {
	private IQ request;
	private ChannelManager channelManager;
	private ItemDelete itemDelete;
	private JID jid = new JID("juliet@shakespeare.lit");
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private String node = "/user/capulet@shakespeare.lit/posts";
	private String payload;

	@Before
	public void setUp() throws Exception {
		channelManager = Mockito.mock(ChannelManager.class);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());

		queue = new LinkedBlockingQueue<Packet>();
		itemDelete = new ItemDelete(queue, channelManager);
		request = readStanzaAsIq("/iq/pubsub/item/delete/request.stanza");

		itemDelete.setServerDomain("shakespeare.lit");

		element = new BaseElement("retract");
		element.addAttribute("node", node);

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.member, null));

		payload = readStanzaAsString("/iq/pubsub/item/item.payload");
	}

	@Test
	public void testPassingRetractAsElementNameReturnsTrue() {
		Element element = new BaseElement("retract");
		Assert.assertTrue(itemDelete.accept(element));
	}

	@Test
	public void testPassingNotRetractAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-retract");
		Assert.assertFalse(itemDelete.accept(element));
	}

	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception {
		Element element = new BaseElement("retract");
		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required",
				error.getApplicationConditionName());
	}

	@Test
	public void testNodeStoreExceptionReturnsErrorStanza() throws Exception {
		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.nodeExists(node);

		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
		Assert.assertEquals(PacketError.Type.wait, error.getType());

	}

	@Test
	public void testProvidingNodeWhichDoesntExistReturnsError()
			throws Exception {
		Mockito.when(channelManager.nodeExists(node)).thenReturn(false);
		itemDelete.setChannelManager(channelManager);

		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void testProvidingInvalidStanzaReturnsError() throws Exception {
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		itemDelete.setChannelManager(channelManager);

		IQ request = toIq(readStanzaAsString("/iq/pubsub/item/delete/request.stanza"));
		request.getChildElement().element("retract").element("item").detach();
		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testNotProvidingItemIdReturnsError() throws Exception {
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		itemDelete.setChannelManager(channelManager);

		IQ request = toIq(readStanzaAsString("/iq/pubsub/item/delete/request.stanza"));
		request.getChildElement().element("retract").element("item")
				.attribute("id").detach();
		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("item-required",
				error.getApplicationConditionName());
	}

	@Test
	public void testProvidingEmptyItemIdReturnsError() throws Exception {
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		itemDelete.setChannelManager(channelManager);

		request = toIq(readStanzaAsString(
				"/iq/pubsub/item/delete/request.stanza").replaceFirst(
				"item-id", ""));
		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("item-required",
				error.getApplicationConditionName());
	}

	@Test
	public void testItemWhichDoesntExistReturnsItemNotFoundError()
			throws Exception {
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeItem(node, "item-id")).thenReturn(
				null);
		itemDelete.setChannelManager(channelManager);

		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void testInvalidPayloadMessageReturnsErrorStanza() throws Exception {
		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload.replaceFirst("<content>", ""), "12345") {
		};
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeItem(node, "item-id")).thenReturn(
				nodeItem);

		itemDelete.setChannelManager(channelManager);

		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.wait, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void userDoesNotOwnItemCanNotDelete() throws Exception {

		String payload = readStanzaAsString("/iq/pubsub/item/item.payload");

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload.replace("juliet@shakespeare.lit",
						"romeo@shakespeare.lit"), "12345");
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeItem(node, "item-id")).thenReturn(
				nodeItem);

		itemDelete.setChannelManager(channelManager);

		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}

	@Test
	public void testUserDoesNotOwnNodeCanNotDelete() throws Exception {

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload, "12345");

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(
				channelManager.getNodeItem(Mockito.anyString(),
						Mockito.anyString())).thenReturn(nodeItem);

		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}

	@Test
	public void testSuccessfulRequestSendsResponseStanza() throws Exception {

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload.replaceAll("romeo@shakespeare.lit",
						"juliet@shakespeare.lit"), "12345");

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(
				channelManager.getNodeItem(Mockito.anyString(),
						Mockito.anyString())).thenReturn(nodeItem);

		itemDelete.setChannelManager(channelManager);

		itemDelete.process(element, jid, request, null);

		Mockito.verify(channelManager).deleteNodeItemById(node, "item-id");
		IQ response = (IQ) queue.poll(100, TimeUnit.MILLISECONDS);

		Assert.assertEquals(IQ.Type.result.toString(), response.getElement()
				.attribute("type").getValue());
		// Check that no notifications are sent
		Packet notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertNull(notification);
	}

	@Test
	public void testRequestingNotificationsSendsRetractionNotifications()
			throws Exception {

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload.replaceAll("romeo@shakespeare.lit",
						"juliet@shakespeare.lit"), "12345");

		request = toIq(readStanzaAsString(
				"/iq/pubsub/item/delete/request.stanza").replaceFirst(
				"<retract", "<retract notify='true'"));

		ArrayList<NodeSubscription> subscriptions = new ArrayList<NodeSubscription>();
		NodeSubscriptionImpl subscription1 = new NodeSubscriptionImpl(node,
				new JID("romeo@shakespeare.lit"), Subscriptions.pending, null);
		NodeSubscriptionImpl subscription2 = new NodeSubscriptionImpl(node,
				new JID("juliet@shakespeare.lit"), Subscriptions.subscribed,
				null);
		subscriptions.add(subscription1);
		subscriptions.add(subscription2);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeItem(node, "item-id")).thenReturn(
				nodeItem);

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscriptions))
				.when(channelManager).getNodeSubscriptionListeners(node);

		itemDelete.setChannelManager(channelManager);

		itemDelete.process(element, jid, request, null);

		Mockito.verify(channelManager).deleteNodeItemById(node, "item-id");
		IQ response = (IQ) queue.poll();

		Assert.assertEquals(IQ.Type.result.toString(), response.getElement()
				.attribute("type").getValue());
		// Check that one notification is sent (on subscriber + 2 admins)
		Assert.assertEquals(3, queue.size());

		Packet notification = queue.poll();
		Assert.assertNotNull(notification);
		Assert.assertEquals("item-id",
				notification.getElement().element("event").element("items")
						.element("retract").attributeValue("id"));
	}

	@Test
	public void testNoNotifyAttributeStillSendsNotifications() throws Exception {

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload.replaceAll("romeo@shakespeare.lit",
						"juliet@shakespeare.lit"), "12345");

		IQ request = toIq(readStanzaAsString("/iq/pubsub/item/delete/request.stanza"));
		request.getChildElement().element("retract").element("item")
				.attribute("notify").detach();

		ArrayList<NodeSubscription> subscriptions = new ArrayList<NodeSubscription>();
		NodeSubscriptionImpl subscription1 = new NodeSubscriptionImpl(node,
				new JID("romeo@shakespeare.lit"), Subscriptions.pending, null);
		NodeSubscriptionImpl subscription2 = new NodeSubscriptionImpl(node,
				new JID("juliet@shakespeare.lit"), Subscriptions.subscribed,
				null);
		subscriptions.add(subscription1);
		subscriptions.add(subscription2);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeItem(node, "item-id")).thenReturn(
				nodeItem);

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscriptions))
				.when(channelManager).getNodeSubscriptionListeners(node);

		itemDelete.setChannelManager(channelManager);

		itemDelete.process(element, jid, request, null);

		Mockito.verify(channelManager).deleteNodeItemById(node, "item-id");

		// Check that one notification is sent (on subscriber + 2 admins)
		Assert.assertEquals(4, queue.size());

	}

	@Test
	public void doesNotRequestThreadWhenDealingWithReply() throws Exception {
		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload, "12345");

		ArrayList<NodeSubscription> subscriptions = new ArrayList<NodeSubscription>();
		NodeSubscriptionImpl subscription1 = new NodeSubscriptionImpl(node,
				new JID("romeo@shakespeare.lit"), Subscriptions.pending, null);
		subscriptions.add(subscription1);

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeItem(node, "item-id")).thenReturn(
				nodeItem);
		Mockito.doThrow(Exception.class)
				.when(channelManager)
				.getNodeItemReplies(Mockito.anyString(), Mockito.anyString(),
						Mockito.anyString(), Mockito.anyInt());

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscriptions))
				.when(channelManager).getNodeSubscriptionListeners(node);
		itemDelete.setChannelManager(channelManager);

		itemDelete.process(element, jid, request, null);
	}
	
	@Test
	public void requestsThreadWhenDeletingParentPost() throws Exception {
		
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.owner, null));
		
		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload);

		ArrayList<NodeSubscription> subscriptions = new ArrayList<NodeSubscription>();

		
		ArrayList<NodeItem> replies = new ArrayList<NodeItem>();
		replies.add(new NodeItemImpl(node, "2", new Date(),
				payload));
		replies.add(new NodeItemImpl(node, "1", new Date(),
				payload));
		

		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeItem(node, "item-id")).thenReturn(
				nodeItem);
		Mockito.when(channelManager.getNodeItemReplies(Mockito.eq(node), Mockito.eq("item-id"),
						Mockito.anyString(), Mockito.eq(-1))).thenReturn(new ClosableIteratorImpl<NodeItem>(replies
								.iterator()));

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscriptions))
				.when(channelManager).getNodeSubscriptionListeners(node);

		itemDelete.process(element, jid, request, null);
		
		Assert.assertEquals(7, queue.size());
		
		Assert.assertEquals(IQ.Type.result, ((IQ) queue.poll()).getType());
		
		Packet notification = queue.poll();

		Assert.assertNotNull(notification);
		Assert.assertEquals("message", notification.getElement().getName());
		Assert.assertEquals("user1@server1", notification.getTo().toString());
		Assert.assertEquals("2",
				notification.getElement().element("event").element("items")
						.element("retract").attributeValue("id"));
		notification = queue.poll();
		Assert.assertNotNull(notification);
		Assert.assertEquals("2",
				notification.getElement().element("event").element("items")
						.element("retract").attributeValue("id"));
		
		notification = queue.poll();
		Assert.assertNotNull(notification);
		Assert.assertEquals("message", notification.getElement().getName());
		Assert.assertEquals("user1@server1", notification.getTo().toString());
		Assert.assertEquals("1",
				notification.getElement().element("event").element("items")
						.element("retract").attributeValue("id"));
		notification = queue.poll();
		Assert.assertNotNull(notification);
		Assert.assertEquals("message", notification.getElement().getName());
		Assert.assertEquals("user2@server1", notification.getTo().toString());
		Assert.assertEquals("1",
				notification.getElement().element("event").element("items")
						.element("retract").attributeValue("id"));
		
		/* Lastly the originally deleted post */
		notification = queue.poll();
		Assert.assertNotNull(notification);
		Assert.assertEquals("message", notification.getElement().getName());
		Assert.assertEquals("user1@server1", notification.getTo().toString());
		Assert.assertEquals("item-id",
				notification.getElement().element("event").element("items")
						.element("retract").attributeValue("id"));
		notification = queue.poll();
		Assert.assertNotNull(notification);
		Assert.assertEquals("item-id",
				notification.getElement().element("event").element("items")
						.element("retract").attributeValue("id"));
	}
}