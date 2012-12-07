package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class ItemDeleteTest extends IQTestHandler {
	private IQ request;
	private ChannelManager channelManagerMock;
	private ItemDelete itemDelete;
	private JID jid;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private String node = "/user/capulet@shakespeare.lit/posts";
	private String payload;

	@Before
	public void setUp() throws Exception {
		channelManagerMock = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManagerMock.isLocalNode(Mockito.anyString()))
				.thenReturn(true);

		queue = new LinkedBlockingQueue<Packet>();
		itemDelete = new ItemDelete(queue, channelManagerMock);
		jid = new JID("juliet@shakespeare.lit");
		request = readStanzaAsIq("/iq/pubsub/item/delete/request.stanza");

		itemDelete.setServerDomain("shakespeare.lit");
		itemDelete.setChannelManager(channelManagerMock);

		element = new BaseElement("retract");
		element.addAttribute("node", node);

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
		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.doThrow(new NodeStoreException()).when(channelManagerMock)
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
		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(false);
		itemDelete.setChannelManager(channelManagerMock);

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
		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		itemDelete.setChannelManager(channelManagerMock);

		request = toIq(readStanzaAsString(
				"/iq/pubsub/item/delete/request.stanza").replaceFirst(
				"<item id='item-id' notify='true' />", ""));
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
		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		itemDelete.setChannelManager(channelManagerMock);

		request = toIq(readStanzaAsString(
				"/iq/pubsub/item/delete/request.stanza").replaceFirst(
				"id='item-id'", ""));
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
		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		itemDelete.setChannelManager(channelManagerMock);

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
		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManagerMock.getNodeItem(node, "item-id"))
				.thenReturn(null);
		itemDelete.setChannelManager(channelManagerMock);

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
				payload.replaceFirst("<content>", ""));
		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManagerMock.getNodeItem(node, "item-id"))
				.thenReturn(nodeItem);

		itemDelete.setChannelManager(channelManagerMock);

		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.wait, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testUserDoesNotOwnItemCanNotDelete() throws Exception {

		String payload = readStanzaAsString("/iq/pubsub/item/item.payload");

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload.replace("juliet@shakespeare.lit",
						"romeo@shakespeare.lit"));
		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManagerMock.getNodeItem(node, "item-id"))
				.thenReturn(nodeItem);

		itemDelete.setChannelManager(channelManagerMock);

		itemDelete.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}

	@Test
	public void testUserDoesNotOwnNodeCanNotDelete() throws Exception {

		NodeAffiliation affiliation = new NodeAffiliationImpl(node, jid,
				Affiliations.member);

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload);

		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				channelManagerMock.getNodeItem(Mockito.anyString(),
						Mockito.anyString())).thenReturn(nodeItem);
		Mockito.when(
				channelManagerMock.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliation);

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
		NodeAffiliation affiliation = new NodeAffiliationImpl(node, jid,
				Affiliations.member);

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload.replaceAll("romeo@shakespeare.lit",
						"juliet@shakespeare.lit"));

		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(
				channelManagerMock.getNodeItem(Mockito.anyString(),
						Mockito.anyString())).thenReturn(nodeItem);
		Mockito.when(
				channelManagerMock.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliation);

		itemDelete.setChannelManager(channelManagerMock);

		itemDelete.process(element, jid, request, null);

		Mockito.verify(channelManagerMock).deleteNodeItemById(node, "item-id");
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
		NodeAffiliation affiliation = new NodeAffiliationImpl(node, jid,
				Affiliations.member);

		NodeItem nodeItem = new NodeItemImpl(node, "item-id", new Date(),
				payload.replaceAll("romeo@shakespeare.lit",
						"juliet@shakespeare.lit"));

		request = toIq(readStanzaAsString(
				"/iq/pubsub/item/delete/request.stanza").replaceFirst(
				"<retract", "<retract notify='true'"));

		ArrayList<NodeSubscription> subscriptions = new ArrayList<NodeSubscription>();
		NodeSubscriptionImpl subscription1 = new NodeSubscriptionImpl(node,
				new JID("romeo@shakespeare.lit"), Subscriptions.pending);
		NodeSubscriptionImpl subscription2 = new NodeSubscriptionImpl(node,
				new JID("juliet@shakespeare.lit"), Subscriptions.subscribed);
		subscriptions.add(subscription1);
		subscriptions.add(subscription2);

		Mockito.when(channelManagerMock.isLocalNode(node)).thenReturn(true);
		Mockito.when(channelManagerMock.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManagerMock.getNodeItem(node, "item-id"))
				.thenReturn(nodeItem);
		Mockito.when(
				channelManagerMock.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliation);

		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscriptions))
				.when(channelManagerMock).getNodeSubscriptionListeners(node);

		itemDelete.setChannelManager(channelManagerMock);

		itemDelete.process(element, jid, request, null);

		Mockito.verify(channelManagerMock).deleteNodeItemById(node, "item-id");
		IQ response = (IQ) queue.poll(100, TimeUnit.MILLISECONDS);

		Assert.assertEquals(IQ.Type.result.toString(), response.getElement()
				.attribute("type").getValue());
		// Check that one notification is sent
		Assert.assertEquals(1, queue.size());

		Packet notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertNotNull(notification);
		Assert.assertEquals("item-id",
				notification.getElement().element("event").element("items")
						.element("retract").attributeValue("id"));
	}
}