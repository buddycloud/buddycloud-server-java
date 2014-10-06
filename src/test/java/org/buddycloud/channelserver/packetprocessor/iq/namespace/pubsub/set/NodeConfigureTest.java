package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
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
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class NodeConfigureTest extends IQTestHandler {
	private IQ request;
	private ChannelManager channelManager;
	private NodeConfigure nodeConfigure;
	private JID jid;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private String node = "/user/juliet@shakespeare.lit/posts";

	@Before
	public void setUp() throws Exception {
		channelManager = Mockito.mock(ChannelManager.class);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());

		queue = new LinkedBlockingQueue<Packet>();
		nodeConfigure = new NodeConfigure(queue, channelManager);
		jid = new JID("juliet@shakespeare.lit");
		request = readStanzaAsIq("/iq/pubsub/channel/configure/request.stanza");

		nodeConfigure.setServerDomain("shakespeare.lit");

		element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespeare.lit/posts");
	}

	@Test
	public void testPassingConfigureAsElementNameReturnsTrue() {
		Element element = new BaseElement("configure");
		Assert.assertTrue(nodeConfigure.accept(element));
	}

	@Test
	public void testPassingNotConfigureAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-configure");
		Assert.assertFalse(nodeConfigure.accept(element));
	}

	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception {
		Element element = new BaseElement("configure");
		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required",
				error.getApplicationConditionName());
	}

	@Test
	public void testNonExistantNodeReturnsErrorStanza() throws Exception {
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/not-here@shakespeare.lit/status");

		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(false);
		nodeConfigure.setChannelManager(channelManager);

		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void userMustBeNodeOwnerToModifyConfiguration() throws Exception {
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");

		Mockito.when(
				channelManager.nodeExists(node))
				.thenReturn(true);
		Mockito.when(
				channelManager.getNodeConfValue(
						"/user/juliet@shakespeare.lit/posts", "pubsub#owner"))
				.thenReturn("romeo@shakespeare.lit");

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed,
						Affiliations.moderator, null));

		nodeConfigure.setChannelManager(channelManager);
		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}

	@Test
	public void testProvidingNoConfigurationDataInStanzaReturnsError()
			throws Exception {
		String nodeId = "/user/juliet@shakespeare.lit/posts";
		String actorJid = "juliet@shakespeare.lit";

		Element element = new BaseElement("configure");
		element.addAttribute("node", nodeId);
		Mockito.when(channelManager.nodeExists(nodeId)).thenReturn(true);
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(nodeId, jid, Subscriptions.subscribed,
						Affiliations.owner, null));

		Helper helper = Mockito.mock(Helper.class);
		Mockito.doThrow(new NodeConfigurationException()).when(helper)
				.parse(request);

		nodeConfigure.setConfigurationHelper(helper);
		nodeConfigure.setChannelManager(channelManager);
		nodeConfigure.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testInvalidConfigurationStanzaReturnsError() throws Exception {

		String nodeId = "/user/juliet@shakespeare.lit/posts";
		String actorJid = "juliet@shakespeare.lit";

		Element element = new BaseElement("configure");
		element.addAttribute("node", nodeId);

		Mockito.when(channelManager.nodeExists(nodeId)).thenReturn(true);
		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(nodeId, jid, Subscriptions.subscribed,
						Affiliations.owner, null));

		Helper helper = Mockito.mock(Helper.class);
		Mockito.when(helper.isValid()).thenReturn(false);

		nodeConfigure.setChannelManager(channelManager);
		nodeConfigure.setConfigurationHelper(helper);
		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testDatabaseErrorOnUpdateConfigurationReturnsError()
			throws Exception {

		String nodeId = "/user/juliet@shakespeare.lit/posts";
		String actorJid = "juliet@shakespeare.lit";

		Element element = new BaseElement("configure");
		element.addAttribute("node", nodeId);

		Mockito.when(channelManager.nodeExists(nodeId)).thenReturn(true);

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(nodeId, jid, Subscriptions.subscribed,
						Affiliations.owner, null));

		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.setNodeConf(Mockito.anyString(), Mockito.any(Map.class));

		Helper helper = Mockito.mock(Helper.class);
		Mockito.when(helper.isValid()).thenReturn(true);

		nodeConfigure.setChannelManager(channelManager);
		nodeConfigure.setConfigurationHelper(helper);
		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testSuccessfulSettingOfConfigurationReturnsConfirmationStanza()
			throws Exception {

		String nodeId = "/user/juliet@shakespeare.lit/posts";
		String actorJid = "juliet@shakespeare.lit";

		Element element = new BaseElement("configure");
		element.addAttribute("node", nodeId);

		Mockito.when(channelManager.nodeExists(nodeId)).thenReturn(true);

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(nodeId, jid, Subscriptions.subscribed,
						Affiliations.owner, null));

		ArrayList<NodeSubscriptionMock> subscribers = new ArrayList<NodeSubscriptionMock>();

		Mockito.doReturn(new ResultSetImpl<NodeSubscriptionMock>(subscribers))
				.when(channelManager)
				.getNodeSubscriptionListeners(Mockito.anyString());

		Helper helper = Mockito.mock(Helper.class);
		Mockito.when(helper.isValid()).thenReturn(true);

		nodeConfigure.setChannelManager(channelManager);
		nodeConfigure.setConfigurationHelper(helper);
		nodeConfigure.process(element, jid, request, null);

		IQ response = (IQ) queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals(IQ.Type.result.toString(), response.getType()
				.toString());
	}

	@Test
	public void testSettingConfigurationUpdatesSubscribers() throws Exception {

		String nodeId = "/user/juliet@shakespeare.lit/posts";
		String actorJid = "juliet@shakespeare.lit";

		Element element = new BaseElement("configure");
		element.addAttribute("node", nodeId);

		Mockito.when(channelManager.nodeExists(nodeId)).thenReturn(true);

		Mockito.when(
				channelManager.getNodeMembership(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(
				new NodeMembershipImpl(nodeId, jid, Subscriptions.subscribed,
						Affiliations.owner, null));

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionMock(new JID(
				"romeo@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"hamlet@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"bottom@shakespeare.lit")));
		ResultSetImpl<NodeSubscription> res = new ResultSetImpl<NodeSubscription>(
				subscribers);
		Mockito.doReturn(res).when(channelManager)
				.getNodeSubscriptionListeners(Mockito.anyString());

		Helper helper = Mockito.mock(Helper.class);
		Mockito.when(helper.isValid()).thenReturn(true);

		nodeConfigure.setChannelManager(channelManager);
		nodeConfigure.setConfigurationHelper(helper);
		nodeConfigure.process(element, jid, request, null);

		queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals(5, queue.size());
		Packet notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("romeo@shakespeare.lit", notification.getTo()
				.toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("hamlet@shakespeare.lit", notification.getTo()
				.toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("bottom@shakespeare.lit", notification.getTo()
				.toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("user1@server1", notification.getTo().toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("user2@server1", notification.getTo().toString());
	}
}