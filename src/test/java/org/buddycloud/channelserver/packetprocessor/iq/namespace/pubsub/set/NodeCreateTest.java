package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.buddycloud.channelserver.channel.node.configuration.HelperMock;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelTitle;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeCreateTest extends IQHandlerTest
{
	private IQ         request;
	private Mock       dataStore;
	private NodeCreate nodeCreate;
	private JID        jid;
	private Element    element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception
	{
		dataStore  = new Mock();
		queue      = new LinkedBlockingQueue<Packet>();
		nodeCreate = new NodeCreate(queue, dataStore);
		jid        = new JID("juliet@shakespeare.lit");
		request    = readStanzaAsIq("/iq/pubsub/channel/create/request.stanza");
		
		nodeCreate.setServerDomain("shakespeare.lit");
		
		element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespeare.lit/posts");
	}

	@Test
	public void testPassingCreateAsElementNameReturnsTrue()
	{
		Element element = new BaseElement("create");
		assertTrue(nodeCreate.accept(element));
	}
	
	@Test
	public void testPassingNotCreateAsElementNameReturnsFalse()
	{
		Element element = new BaseElement("not-create");
		assertFalse(nodeCreate.accept(element));
	}
	
	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception
	{
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
        throws Exception
	{
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito
		    .when(dataStoreMock.nodeExists("/user/capulet@shakespeare.lit/posts"))
		    .thenReturn(true);
        nodeCreate.setDataStore(dataStoreMock);

		nodeCreate.process(element, jid, request, null);
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.cancel, error.getType());
		assertEquals(PacketError.Condition.conflict, error.getCondition());
		/**
		 * Add this check back in once Tinder supports xmlns on standard conditions
		 * assertEquals(JabberPubsub.NS_XMPP_STANZAS, error.getApplicationConditionNamespaceURI());
		 */
	}
	
	@Test
	public void testUnauthenticatedUserCanNotCreateNode()
	    throws Exception
	{
		JID jid = new JID("juliet@anon.shakespeare.lit");

		nodeCreate.process(element, jid, request,  null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Condition.forbidden, error.getCondition());
		assertEquals(PacketError.Type.auth, error.getType());
		/**
		 * Add this check back in once Tinder supports xmlns on standard conditions
		 * assertEquals(JabberPubsub.NS_XMPP_STANZAS, error.getApplicationConditionNamespaceURI());
		 */
	}
	
	@Test
	public void testInvalidlyFormattedNodeReturnsError() throws Exception
	{
		element.addAttribute("node", "/user/capulet@shakespeare/posts/invalid");
		
		nodeCreate.process(element, jid, request,  null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
		/**
		 * Add this check back in once Tinder supports xmlns on standard conditions
		 * assertEquals(JabberPubsub.NS_XMPP_STANZAS, error.getApplicationConditionNamespaceURI());
		 */
	}
	
	@Test
	public void testNewNodeMustBeOnADomainSupportedByCurrentServer() throws Exception
	{
		element.addAttribute("node", "/user/capulet@shakespearelit/posts");

		nodeCreate.setTopicsDomain("topics.shakespeare.lit");

		nodeCreate.process(element, jid, request,  null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.not_acceptable, error.getCondition());
		/**
		 * Add this check back in once Tinder supports xmlns on standard conditions
		 * assertEquals(JabberPubsub.NS_XMPP_STANZAS, error.getApplicationConditionNamespaceURI());
		 */
	}
	
	@Test
	public void testDataStoreFailureReturnsInternalServerErrorResponse() 
		throws Exception
	{
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.doThrow(new DataStoreException())
		    .when(dataStoreMock)
		    .createNode(
		        Mockito.anyString(), 
		        Mockito.anyString(), 
		        Mockito.anyMapOf(String.class, String.class)
		    );
		nodeCreate.setDataStore(dataStoreMock);

		nodeCreate.process(element, jid, request,  null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Condition.internal_server_error, error.getCondition());
		assertEquals(PacketError.Type.wait, error.getType());
		/**
		 * Add this check back in once Tinder supports xmlns on standard conditions
		 * assertEquals(JabberPubsub.NS_XMPP_STANZAS, error.getApplicationConditionNamespaceURI());
		 */
	}

	@Test
	public void testValidCreateNodeRequestReturnsConfirmationStanza() throws Exception
	{
		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito
		    .when(helperMock.parse(request))
		    .thenReturn(null);
        nodeCreate.setConfigurationHelper(helperMock);

		nodeCreate.process(element, jid, request,  null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		String error    = null;
		try {
			error = response.getError().toString();
			System.out.println(error.toString());
			fail("Unexpected error response");
		} catch (NullPointerException e) {
			assertNull(error);
		}
		assertEquals(
		    IQ.Type.result.toString(),
		    response.getElement().attribute("type").getValue()
		);
	}
	
	@Test
	public void testCreateNodeWithConfigurationResultsInExpectedConfig() 
	    throws Exception
	{	
		String channelTitle = "test-channel-name";

		HashMap<String, String> configurationProperties = new HashMap<String, String>();
		configurationProperties.put(ChannelTitle.FIELD_NAME, channelTitle);

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito
		    .when(helperMock.parse(request))
		    .thenReturn(configurationProperties);
        nodeCreate.setConfigurationHelper(helperMock);

		nodeCreate.process(element, jid, request,  null);
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		String error    = null;
		try {
			error = response.getError().toString();
			System.out.println(error.toString());
			fail("Unexpected error response");
		} catch (NullPointerException e) {
			assertNull(error);
		}
		Map<String, String> nodeConfiguration = dataStore.getConfiguration();
		assertEquals(channelTitle, nodeConfiguration.get(ChannelTitle.FIELD_NAME));		
	}
	
	@Test 
	public void testFailingNodeConfigurationReturnsErrorStanza() throws Exception
	{
		String channelTitle = "test-channel-name";

		HashMap<String, String> configurationProperties = new HashMap<String, String>();
		configurationProperties.put(ChannelTitle.FIELD_NAME, channelTitle);

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito
		    .when(helperMock.parse(request))
		    .thenThrow(new NodeConfigurationException());
        nodeCreate.setConfigurationHelper(helperMock);

		nodeCreate.process(element, jid, request,  null);
		
		Packet response   = queue.poll(100, TimeUnit.MILLISECONDS);
		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals(PacketError.Condition.bad_request, error.getCondition());
		/**
		 * Add this check back in once Tinder supports xmlns on standard conditions
		 * assertEquals(JabberPubsub.NS_XMPP_STANZAS, error.getApplicationConditionNamespaceURI());
		 */
	}
}