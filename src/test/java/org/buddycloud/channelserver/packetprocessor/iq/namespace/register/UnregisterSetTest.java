package org.buddycloud.channelserver.packetprocessor.iq.namespace.register;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.NodeStore.Transaction;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
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
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class UnregisterSetTest extends IQTestHandler {

	private ChannelManager channelManager;
	private LinkedBlockingQueue<Packet> queue;
	private UnregisterSet unregisterSet;

	@Before
	public void setUp() throws Exception {
		this.channelManager = Mockito.mock(ChannelManager.class);
		this.queue = new LinkedBlockingQueue<Packet>();
		this.unregisterSet = new UnregisterSet(queue, channelManager);
	}

	private void recordEmptyMockResponses(JID actorJid)
			throws NodeStoreException {
		Mockito.when(
				channelManager.nodeExists(Mockito.eq("/user/" + actorJid.toBareJID()
						+ "/posts"))).thenReturn(true);
		Mockito.when(channelManager.getUserMemberships(Mockito.eq(actorJid))).thenReturn(
				new ResultSetImpl<NodeMembership>(
						new LinkedList<NodeMembership>()));
		Mockito.when(channelManager.getUserMemberships(Mockito.eq(actorJid))).thenReturn(
				new ResultSetImpl<NodeMembership>(
						new LinkedList<NodeMembership>()));
		Mockito.when(channelManager.getUserItems(Mockito.eq(actorJid))).thenReturn(
				new ResultSetImpl<NodeItem>(new LinkedList<NodeItem>()));
		Mockito.when(channelManager.beginTransaction()).thenReturn(
				Mockito.mock(Transaction.class));
	}

	@Test
	public void testRemoteRequestWrongDomain() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/fail/remote-wrong-domain.stanza");
		unregisterSet.process(request);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testBadFormattedStanza() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/fail/bad-formatted.stanza");
		unregisterSet.process(request);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testUnregisteredUser() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		unregisterSet.process(request);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.registration_required,
				error.getCondition());
	}

	@Test
	public void testRegisteredUserNoNotifications() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		JID actorJid = request.getFrom();

		recordEmptyMockResponses(actorJid);

		unregisterSet.process(request);
		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());
		Assert.assertEquals(Type.result, response.getType());
		Assert.assertTrue(queue.isEmpty());
	}

	@Test
	public void notifyDeletePersonalNode() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		JID actorJid = request.getFrom();

		recordEmptyMockResponses(actorJid);

		String personalNode = "/user/" + actorJid.toBareJID() + "/posts";

		// Record affiliations
		NodeMembership membership = new NodeMembershipImpl(personalNode,
				actorJid, actorJid, Subscriptions.subscribed,
				Affiliations.owner);
		List<NodeMembership> memberships = new LinkedList<NodeMembership>();
		memberships.add(membership);
		Mockito.when(channelManager.getUserMemberships(Mockito.eq(actorJid))).thenReturn(
				new ResultSetImpl<NodeMembership>(memberships));

		// Record local node
		Mockito.when(channelManager.isLocalNode(Mockito.eq(personalNode))).thenReturn(true);

		// Record channel type
		Mockito.when(
				channelManager
						.getNodeConfValue(personalNode, Conf.CHANNEL_TYPE))
				.thenReturn("personal");

		unregisterSet.process(request);

		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());
		Assert.assertEquals(Type.result, response.getType());
		Assert.assertFalse(queue.isEmpty());

		int adminCount = Configuration.getInstance().getAdminUsers().size() * 2;
		Assert.assertEquals(adminCount, queue.size());

		Packet deleteNodeNotification = queue.poll();
		Element deleteEl = deleteNodeNotification.getElement().element("event")
				.element("delete");
		Assert.assertEquals(personalNode, deleteEl.attributeValue("node"));
	}

	@Test
	public void dontNotifyDeleteRemoteTopicNode() throws Exception {

		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		JID actorJid = request.getFrom();

		recordEmptyMockResponses(actorJid);

		String topicNode = "/user/topic@shakespeare.lit/posts";

		// Record affiliations
		NodeMembership membership = new NodeMembershipImpl(topicNode,
				actorJid, actorJid, Subscriptions.subscribed, Affiliations.owner, new Date());
		
		List<NodeMembership> memberships = new LinkedList<NodeMembership>();
		memberships.add(membership);
		
		Mockito.when(channelManager.getUserMemberships(actorJid)).thenReturn(
				new ResultSetImpl<NodeMembership>(memberships));
		
		Mockito.when(
				channelManager.getNodeMemberships(Mockito.eq(topicNode))).thenReturn(
				new ResultSetImpl<NodeMembership>(memberships));

		Mockito.when(channelManager.isLocalNode(Mockito.anyString())).thenReturn(false);

		// Record channel type
		Mockito.when(
				channelManager.getNodeConfValue(topicNode, Conf.CHANNEL_TYPE))
				.thenReturn("topic");

		unregisterSet.process(request);

		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());
		Assert.assertEquals(Type.result, response.getType());

		Assert.assertEquals(2, queue.size());
	}

	@Test
	public void notifyDeleteLocalTopicNode() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		JID actorJid = request.getFrom();

		recordEmptyMockResponses(actorJid);

		String topicNode = "/user/topic@shakespeare.lit/posts";

		// Record affiliations
		NodeMembership membership = new NodeMembershipImpl(topicNode,
				actorJid, actorJid, Subscriptions.subscribed, Affiliations.owner, new Date());
		List<NodeMembership> memberships = new LinkedList<NodeMembership>();
		memberships.add(membership);
		Mockito.when(channelManager.getUserMemberships(actorJid)).thenReturn(
				new ResultSetImpl<NodeMembership>(memberships));
		Mockito.when(
				channelManager.getNodeMemberships(Mockito.eq(topicNode))).thenReturn(
				new ResultSetImpl<NodeMembership>(memberships));

		// Record local node
		Mockito.when(channelManager.isLocalNode(topicNode)).thenReturn(true);

		// Record channel type
		Mockito.when(
				channelManager.getNodeConfValue(topicNode, Conf.CHANNEL_TYPE))
				.thenReturn("topic");

		unregisterSet.process(request);

		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());
		Assert.assertEquals(Type.result, response.getType());
		Assert.assertFalse(queue.isEmpty());
		
		// Two delete notifications, two subscription notifications
		int adminCount = Configuration.getInstance().getAdminUsers().size() * 2;
		Assert.assertEquals(adminCount, queue.size());

		Packet deleteNodeNotification = queue.poll();
		Element deleteEl = deleteNodeNotification.getElement().element("event")
				.element("delete");
		Assert.assertEquals(topicNode, deleteEl.attributeValue("node"));
	}

	@Test
	public void testDontNotifyDeleteRemoteNodeItem() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		JID actorJid = request.getFrom();

		recordEmptyMockResponses(actorJid);

		String topicNode = "/user/topic@shakespeare.lit/posts";

		// Record remote node
		Mockito.when(channelManager.isLocalNode(topicNode)).thenReturn(false);

		// Record node items
		NodeItem nodeItem = new NodeItemImpl(topicNode, "entry1", new Date(),
				"<payload/>");
		List<NodeItem> nodeItems = new LinkedList<NodeItem>();
		nodeItems.add(nodeItem);
		Mockito.when(channelManager.getUserItems(actorJid)).thenReturn(
				new ResultSetImpl<NodeItem>(nodeItems));

		unregisterSet.process(request);

		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());
		Assert.assertEquals(Type.result, response.getType());
		Assert.assertTrue(queue.isEmpty());
	}

	@Test
	public void testNotifyDeleteLocalNodeItem() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		JID actorJid = request.getFrom();

		recordEmptyMockResponses(actorJid);

		String topicNode = "/user/topic@shakespeare.lit/posts";

		// Record remote node
		Mockito.when(channelManager.isLocalNode(topicNode)).thenReturn(true);

		// Record node items
		String itemId = "entry1";
		NodeItem nodeItem = new NodeItemImpl(topicNode, itemId, new Date(),
				"<payload/>");
		List<NodeItem> nodeItems = new LinkedList<NodeItem>();
		nodeItems.add(nodeItem);
		Mockito.when(channelManager.getUserItems(actorJid)).thenReturn(
				new ResultSetImpl<NodeItem>(nodeItems));

		unregisterSet.process(request);

		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());
		Assert.assertEquals(Type.result, response.getType());
		Assert.assertFalse(queue.isEmpty());

		int adminCount = Configuration.getInstance().getAdminUsers().size();
		Assert.assertEquals(adminCount, queue.size());

		Packet deleteNodeNotification = queue.poll();
		Element itemsEl = deleteNodeNotification.getElement().element("event")
				.element("items");
		Assert.assertEquals(topicNode, itemsEl.attributeValue("node"));

		Element retractEl = itemsEl.element("retract");
		Assert.assertEquals(itemId, retractEl.attributeValue("id"));
	}

	@Test
	public void testNotifyDeleteLocalSubscriptions() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		JID actorJid = request.getFrom();

		recordEmptyMockResponses(actorJid);

		String topicNode = "/user/topic@shakespeare.lit/posts";

		// Record subscriptions
		NodeMembership membership = new NodeMembershipImpl(topicNode,
				actorJid, Subscriptions.subscribed, Affiliations.member);
		List<NodeMembership> memberships = new LinkedList<NodeMembership>();
		memberships.add(membership);
		Mockito.when(channelManager.getUserMemberships(Mockito.eq(actorJid))).thenReturn(
				new ResultSetImpl<NodeMembership>(memberships));

		List<NodeMembership> nodeMemberships = new LinkedList<NodeMembership>();
		nodeMemberships.add(membership);
		Mockito.when(channelManager.getNodeMemberships(Mockito.anyString())).thenReturn(
				new ResultSetImpl<NodeMembership>(nodeMemberships));
		
		unregisterSet.process(request);

		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());
		Assert.assertEquals(Type.result, response.getType());
		Assert.assertFalse(queue.isEmpty());

		int adminCount = Configuration.getInstance().getAdminUsers().size();
		Assert.assertEquals(adminCount, queue.size());

		Packet deleteNodeNotification = queue.poll();
		Element itemsEl = deleteNodeNotification.getElement().element("event")
				.element("subscription");
		Assert.assertEquals(topicNode, itemsEl.attributeValue("node"));
		Assert.assertEquals(actorJid.toBareJID(), itemsEl.attributeValue("jid"));
		Assert.assertEquals("none", itemsEl.attributeValue("subscription"));
	}

	@Test
	public void testForwardRemoteRequests() throws Exception {
		IQ request = readStanzaAsIq("/iq/unregister/local-request.stanza");
		JID actorJid = request.getFrom();

		recordEmptyMockResponses(actorJid);

		String remoteNode = "/user/remotenode@remotedomain.com/posts";
		ArrayList<String> remoteNodes = new ArrayList<String>();
		remoteNodes.add(remoteNode);
		Mockito.when(channelManager.getNodeList()).thenReturn(remoteNodes);

		unregisterSet.process(request);

		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());
		Assert.assertEquals(Type.result, response.getType());
		Assert.assertFalse(queue.isEmpty());

		Assert.assertEquals(1, queue.size());

		Packet remoteRequest = queue.poll();
		Element removeEl = remoteRequest.getElement().element("query")
				.element("remove");
		Assert.assertNotNull(removeEl);
		Element actorEl = removeEl.element("actor");
		Assert.assertEquals(actorJid.toBareJID(), actorEl.getText());
	}
}
