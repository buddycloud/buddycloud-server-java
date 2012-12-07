package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.node.configuration.HelperMock;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliation;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscriptionMock;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.Result;
import org.xmpp.resultsetmanagement.ResultSet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class NodeConfigureTest extends IQTestHandler {
	private IQ request;
	private ChannelManager channelManagerMock;
	private NodeConfigure nodeConfigure;
	private JID jid;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception {
		channelManagerMock = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManagerMock.isLocalNode(Mockito.anyString())).thenReturn(true);
		
		queue = new LinkedBlockingQueue<Packet>();
		nodeConfigure = new NodeConfigure(queue, channelManagerMock);
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
		Assert.assertEquals("nodeid-required", error.getApplicationConditionName());
	}

	@Test
	public void testNonExistantNodeReturnsErrorStanza() throws Exception {
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/not-here@shakespeare.lit/status");

		Mockito.when(
				channelManagerMock
						.nodeExists(Mockito.anyString()))
				.thenReturn(false);
		nodeConfigure.setChannelManager(channelManagerMock);

		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}

	@Test
	public void testUserMustBeNodeOwnerToModifyConfiguration() throws Exception {
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");

		Mockito.when(
				channelManagerMock
						.nodeExists("/user/juliet@shakespeare.lit/posts"))
				.thenReturn(true);
		Mockito.when(
				channelManagerMock.getNodeConfValue(
						"/user/juliet@shakespeare.lit/posts", "pubsub#owner"))
				.thenReturn("romeo@shakespeare.lit");

		nodeConfigure.setChannelManager(channelManagerMock);
		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
	}

	@Test
	public void testProvidingNoConfigurationDataInStanzaReturnsError()
			throws Exception {
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");

		Mockito.when(
				channelManagerMock
						.nodeExists("/user/juliet@shakespeare.lit/posts"))
				.thenReturn(true);
		Mockito.when(
				channelManagerMock.getNodeConfValue(
						"/user/juliet@shakespeare.lit/posts", "pubsub#owner"))
				.thenReturn("juliet@shakespeare.lit");

		Helper helperMock = Mockito.mock(Helper.class);
		Mockito.doThrow(new NodeConfigurationException()).when(helperMock)
				.parse(request);

		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.setChannelManager(channelManagerMock);
		nodeConfigure.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}

	@Test
	public void testInvalidConfigurationStanzaReturnsError() throws Exception {
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");

		Mockito.when(
				channelManagerMock
						.nodeExists("/user/juliet@shakespeare.lit/posts"))
				.thenReturn(true);
		Mockito.when(
				channelManagerMock.getNodeConfValue(
						"/user/juliet@shakespeare.lit/posts", "pubsub#owner"))
				.thenReturn("juliet@shakespeare.lit");

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito.when(helperMock.isValid()).thenReturn(false);

		nodeConfigure.setChannelManager(channelManagerMock);
		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}

	@Test
	public void testDatabaseErrorOnUpdateConfigurationReturnsError()
			throws Exception {
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");

		Mockito.when(
				channelManagerMock
						.nodeExists("/user/juliet@shakespeare.lit/posts"))
				.thenReturn(true);
		Mockito.when(
				channelManagerMock.getNodeConfValue(
						"/user/juliet@shakespeare.lit/posts", "pubsub#owner"))
				.thenReturn("juliet@shakespeare.lit");
		Mockito.doThrow(new NodeStoreException()).when(channelManagerMock)
				.setNodeConf(Mockito.anyString(), Mockito.any(Map.class));

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito.when(helperMock.isValid()).thenReturn(true);

		nodeConfigure.setChannelManager(channelManagerMock);
		nodeConfigure.setConfigurationHelper(helperMock);
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
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");

		Mockito.when(
				channelManagerMock
						.nodeExists("/user/juliet@shakespeare.lit/posts"))
				.thenReturn(true);
		Mockito.when(
				channelManagerMock.getNodeConfValue(
						"/user/juliet@shakespeare.lit/posts", "pubsub#owner"))
				.thenReturn("juliet@shakespeare.lit");

		ArrayList<NodeSubscriptionMock> subscribers = new ArrayList<NodeSubscriptionMock>();
		
		Mockito.doReturn(new ResultSetImpl(subscribers)).when(channelManagerMock)
				.getNodeSubscriptionListeners(Mockito.anyString());

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito.when(helperMock.isValid()).thenReturn(true);

		nodeConfigure.setChannelManager(channelManagerMock);
		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.process(element, jid, request, null);

		IQ response = (IQ) queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals(IQ.Type.result.toString(), response.getType().toString());
	}

	@Test
	public void testSettingConfigurationUpdatesSubscribers() throws Exception {
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");

		Mockito.when(
				channelManagerMock
						.nodeExists("/user/juliet@shakespeare.lit/posts"))
				.thenReturn(true);
		Mockito.when(
				channelManagerMock.getNodeConfValue(
						"/user/juliet@shakespeare.lit/posts", "pubsub#owner"))
				.thenReturn("juliet@shakespeare.lit");

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionMock(new JID(
				"romeo@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"hamlet@shakespeare.lit")));
		subscribers.add(new NodeSubscriptionMock(new JID(
				"bottom@shakespeare.lit")));
        ResultSetImpl<NodeSubscription> res = new ResultSetImpl<NodeSubscription>(subscribers);
		Mockito.doReturn(res).when(channelManagerMock)
				.getNodeSubscriptionListeners(Mockito.anyString());

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito.when(helperMock.isValid()).thenReturn(true);

		nodeConfigure.setChannelManager(channelManagerMock);
		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.process(element, jid, request, null);

		queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals(3, queue.size());
		Packet notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("romeo@shakespeare.lit", notification.getTo().toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("hamlet@shakespeare.lit", notification.getTo().toString());
		notification = queue.poll(100, TimeUnit.MILLISECONDS);
		Assert.assertEquals("bottom@shakespeare.lit", notification.getTo().toString());
	}
}