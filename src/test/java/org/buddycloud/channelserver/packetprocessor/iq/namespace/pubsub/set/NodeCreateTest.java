package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import static org.junit.Assert.fail;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.dom4j.tree.BaseElement;

import java.util.HashMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
 
import org.dom4j.Attribute;
import org.mockito.Mockito;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.NodeCreate;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.IQ;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;

public class NodeCreateTest extends IQHandlerTest
{
	private IQ         stanza;
	private DataStore  dataStore;
	private NodeCreate nodeCreate;
	private JID        jid;
	private IQ         request;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception
	{
		dataStore  = new Mock();
		queue      = new LinkedBlockingQueue<Packet>();
		nodeCreate = new NodeCreate(queue, dataStore);
		jid        = new JID("juliet@capulet.lit");
		request    = readStanzaAsIq("/iq/pubsub/channel/create/request.stanza");
		
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
		Element element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespeare.lit/posts");
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
		Element element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespeare.lit/posts");

		JID jid = new JID("juliet@anon.shakespeare.lit");
		nodeCreate.setServerDomain("shakespeare.lit");
		
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
		Element element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespeare/posts/invalid");
		
		JID jid = new JID("juliet@shakespeare.lit");
		nodeCreate.setServerDomain("shakespeare.lit");

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
		Element element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespearelit/posts");
		
		JID jid = new JID("juliet@shakespeare.lit");
		nodeCreate.setServerDomain("shakespeare.lit");
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
		String node = "/user/capulet@shakespeare.lit/posts";
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.doThrow(new DataStoreException())
		    .when(dataStoreMock)
		    .createNode(Mockito.anyString(), Mockito.anyString(), Mockito.anyMapOf(String.class, String.class));
	        
		nodeCreate.setDataStore(dataStoreMock);

		Element element = new BaseElement("create");
		element.addAttribute("node", node);
		
		JID jid = new JID("juliet@shakespeare.lit");
		nodeCreate.setServerDomain("shakespeare.lit");

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
		Element element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespeare.lit/posts");
		
		JID jid = new JID("juliet@shakespeare.lit");
		nodeCreate.setServerDomain("shakespeare.lit");

		nodeCreate.process(element, jid, request,  null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		assertNull(response.getError());
		assertEquals(IQ.Type.result, ((IQ) response).getType());
	}
}