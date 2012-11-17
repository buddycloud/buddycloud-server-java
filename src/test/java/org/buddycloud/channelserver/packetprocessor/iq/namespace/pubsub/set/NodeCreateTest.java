package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.node.configuration.HelperMock;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelTitle;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeCreateTest extends IQTestHandler {
	private IQ request;
	private Mock channelManager;
	private NodeCreate nodeCreate;
	private JID jid;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private String node = "/user/capulet@shakespeare.lit/posts";

	private ChannelManager channelManagerMock;

	@Before
	public void setUp() throws Exception {
		channelManagerMock = Mockito.mock(Mock.class);
		Mockito.when(channelManagerMock.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		
		queue = new LinkedBlockingQueue<Packet>();
		nodeCreate = new NodeCreate(queue, channelManagerMock);
		jid = new JID("juliet@shakespeare.lit");
		request = readStanzaAsIq("/iq/pubsub/channel/create/request.stanza");

		nodeCreate.setServerDomain("shakespeare.lit");

		element = new BaseElement("create");
		element.addAttribute("node", node);


	}

	@Test
	public void testPassingCreateAsElementNameReturnsTrue() {
		Element element = new BaseElement("create");
		assertTrue(nodeCreate.accept(element));
	}

	@Test
	public void testPassingNotCreateAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-create");
		assertFalse(nodeCreate.accept(element));
	}

	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception {
		Element element = new BaseElement("create");
		nodeCreate.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}

	@Test
	public void testRequestingAlreadyExistingNodeReturnsErrorStanza()
			throws Exception {

		Mockito.when(
				channelManagerMock
						.nodeExists("/user/capulet@shakespeare.lit/posts"))
				.thenReturn(true);
		nodeCreate.setChannelManager(channelManagerMock);

		nodeCreate.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.cancel, error.getType());
		assertEquals(PacketError.Condition.conflict, error.getCondition());
		/**
		 * Add this check back in once Tinder supports xmlns on standard
		 * conditions assertEquals(JabberPubsub.NS_XMPP_STANZAS,
		 * error.getApplicationConditionNamespaceURI());
		 */
	}

	@Test
	public void testUnauthenticatedUserCanNotCreateNode() throws Exception {
		JID jid = new JID("juliet@anon.shakespeare.lit");

		nodeCreate.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Condition.forbidden, error.getCondition());
		assertEquals(PacketError.Type.auth, error.getType());
		/**
		 * Add this check back in once Tinder supports xmlns on standard
		 * conditions assertEquals(JabberPubsub.NS_XMPP_STANZAS,
		 * error.getApplicationConditionNamespaceURI());
		 */
	}

	@Test
	public void testInvalidlyFormattedNodeReturnsError() throws Exception {
		element.addAttribute("node", "/user/capulet@shakespeare/posts/invalid");

		nodeCreate.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
		/**
		 * Add this check back in once Tinder supports xmlns on standard
		 * conditions assertEquals(JabberPubsub.NS_XMPP_STANZAS,
		 * error.getApplicationConditionNamespaceURI());
		 */
	}

	@Test
	public void testNewNodeMustBeOnADomainSupportedByCurrentServer()
			throws Exception {
		element.addAttribute("node", "/user/capulet@shakespearelit/posts");

		nodeCreate.setTopicsDomain("topics.shakespeare.lit");

		nodeCreate.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.not_acceptable, error.getCondition());
		/**
		 * Add this check back in once Tinder supports xmlns on standard
		 * conditions assertEquals(JabberPubsub.NS_XMPP_STANZAS,
		 * error.getApplicationConditionNamespaceURI());
		 */
	}

	@Test
	public void testchannelManagerFailureReturnsInternalServerErrorResponse()
			throws Exception {
		Mockito.doThrow(new NodeStoreException())
				.when(channelManagerMock)
				.createNode(Mockito.any(JID.class), Mockito.anyString(),
						Mockito.anyMapOf(String.class, String.class));
		nodeCreate.setChannelManager(channelManagerMock);
		Helper helperMock = Mockito.mock(Helper.class);
		Mockito.doReturn(true).when(helperMock).isValid();
		nodeCreate.setConfigurationHelper(helperMock);

		nodeCreate.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
		assertEquals(PacketError.Type.wait, error.getType());
		/**
		 * Add this check back in once Tinder supports xmlns on standard
		 * conditions assertEquals(JabberPubsub.NS_XMPP_STANZAS,
		 * error.getApplicationConditionNamespaceURI());
		 */
	}

	@Test
	public void testValidCreateNodeRequestReturnsConfirmationStanza()
			throws Exception {
		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito.doReturn(true).when(helperMock).isValid();
		nodeCreate.setConfigurationHelper(helperMock);

		nodeCreate.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		String error = null;
		try {
			error = response.getError().toString();
			fail("Unexpected error response");
		} catch (NullPointerException e) {
			assertNull(error);
		}
		assertEquals(IQ.Type.result.toString(), response.getElement()
				.attribute("type").getValue());
	}

	@Test
	public void testCreateNodeWithConfigurationResultsInExpectedConfig()
			throws Exception {
		String channelTitle = "test-channel-name";

		HashMap<String, String> configurationProperties = new HashMap<String, String>();
		configurationProperties.put(ChannelTitle.FIELD_NAME, channelTitle);

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito.when(helperMock.getValues())
				.thenReturn(configurationProperties);
		Mockito.doReturn(true).when(helperMock).isValid();

		ChannelManager channelManagerMock = new Mock();
		
		nodeCreate.setChannelManager(channelManagerMock);
		nodeCreate.setConfigurationHelper(helperMock);

		nodeCreate.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		String error = null;
		try {
			error = response.getError().toString();
			fail("Unexpected error response");
		} catch (NullPointerException e) {
			assertNull(error);
		}
		Map<String, String> nodeConfiguration = channelManagerMock
				.getNodeConf(node);
		assertEquals(channelTitle,
				nodeConfiguration.get(ChannelTitle.FIELD_NAME));
	}

	@Test
	public void testFailingNodeConfigurationReturnsErrorStanza()
			throws Exception {
		String channelTitle = "test-channel-name";

		HashMap<String, String> configurationProperties = new HashMap<String, String>();
		configurationProperties.put(ChannelTitle.FIELD_NAME, channelTitle);

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito.doThrow(new NodeConfigurationException()).when(helperMock)
				.parse(request);
		nodeCreate.setConfigurationHelper(helperMock);

		nodeCreate.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
		/**
		 * Add this check back in once Tinder supports xmlns on standard
		 * conditions assertEquals(JabberPubsub.NS_XMPP_STANZAS,
		 * error.getApplicationConditionNamespaceURI());
		 */
	}
}