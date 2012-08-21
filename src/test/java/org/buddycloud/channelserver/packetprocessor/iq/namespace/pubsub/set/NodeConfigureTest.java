package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.node.configuration.HelperMock;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliation;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
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

public class NodeConfigureTest extends IQHandlerTest
{
	private IQ            request;
	private Mock          dataStore;
	private NodeConfigure nodeConfigure;
	private JID           jid;
	private Element       element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	@Before
	public void setUp() throws Exception
	{
		dataStore     = new Mock();
		queue         = new LinkedBlockingQueue<Packet>();
	    nodeConfigure = new NodeConfigure(queue, dataStore);
		jid           = new JID("juliet@shakespeare.lit");
		request       = readStanzaAsIq("/iq/pubsub/channel/configure/request.stanza");
		
		nodeConfigure.setServerDomain("shakespeare.lit");
		
		element = new BaseElement("create");
		element.addAttribute("node", "/user/capulet@shakespeare.lit/posts");
	}

	@Test
	public void testPassingConfigureAsElementNameReturnsTrue()
	{
		Element element = new BaseElement("configure");
		assertTrue(nodeConfigure.accept(element));
	}
	
	@Test
	public void testPassingNotConfigureAsElementNameReturnsFalse()
	{
		Element element = new BaseElement("not-configure");
		assertFalse(nodeConfigure.accept(element));
	}
	
	@Test
	public void testPassingNoNodeResultsInErrorStanza() throws Exception
	{
		Element element = new BaseElement("configure");
		nodeConfigure.process(element, jid, request, null);

		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

	    PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}
	
	@Test
	public void testNonExistantNodeReturnsErrorStanza() throws Exception
	{
		Element element = new BaseElement("configure");
	    element.addAttribute("node", "/user/not-here@shakespeare.lit/status");
	    
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito
		    .when(dataStoreMock.nodeExists("/user/not-here@shakespeare.lit/status"))
		    .thenReturn(false);
        nodeConfigure.setDataStore(dataStoreMock);

        nodeConfigure.process(element, jid, request, null);
        
	    Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
	    PacketError error = response.getError();
	    assertNotNull(error);
	    assertEquals(PacketError.Type.cancel, error.getType());
	    assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}
	
	@Test
	public void testUserMustBeNodeOwnerToModifyConfiguration() throws Exception
	{
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");
		
		HashMap<String, String> nodeProperties = new HashMap<String, String>();
		nodeProperties.put(Affiliation.OWNER, "romeo@shakespeare.lit");
		
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito
		    .when(dataStoreMock.nodeExists("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(true);
		Mockito
            .when(dataStoreMock.getNodeConf("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(nodeProperties);
	
		nodeConfigure.setDataStore(dataStoreMock);
	    nodeConfigure.process(element, jid, request, null);
	    
	    Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
	    PacketError error = response.getError();
	    assertNotNull(error);
	    assertEquals(PacketError.Type.auth, error.getType());
	    assertEquals(PacketError.Condition.forbidden, error.getCondition());
	}
	
	@Test
	public void testProvidingNoConfigurationDataInStanzaReturnsError() throws Exception
	{
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");
		
		HashMap<String, String> nodeProperties = new HashMap<String, String>();
		nodeProperties.put(Affiliation.OWNER, "juliet@shakespeare.lit");
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		
		Mockito
		    .when(dataStoreMock.nodeExists("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(true);
		Mockito
            .when(dataStoreMock.getNodeConf("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(nodeProperties);
        
		Helper helperMock = Mockito.mock(Helper.class);
		Mockito
		    .when(helperMock.parse(request))
		    .thenThrow(new NodeConfigurationException());
		
		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.setDataStore(dataStoreMock);
		nodeConfigure.process(
		    element,
		    jid,
		    request,
		    null
		);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
	    PacketError error = response.getError();
	    assertNotNull(error);
	    assertEquals(PacketError.Type.modify, error.getType());
	    assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}
	
	@Test
	public void testInvalidConfigurationStanzaReturnsError() throws Exception
	{
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");
		
		HashMap<String, String> nodeProperties = new HashMap<String, String>();
		nodeProperties.put(Affiliation.OWNER, "juliet@shakespeare.lit");
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		
		Mockito
		    .when(dataStoreMock.nodeExists("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(true);
		Mockito
            .when(dataStoreMock.getNodeConf("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(nodeProperties);
		
		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito
		    .when(helperMock.isValid())
		    .thenReturn(false);
        
		nodeConfigure.setDataStore(dataStoreMock);
		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.process(element, jid, request,  null);
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
	    PacketError error = response.getError();
	    assertNotNull(error);
	    assertEquals(PacketError.Type.modify, error.getType());
	    assertEquals(PacketError.Condition.bad_request, error.getCondition());
	}
	
	@Test
	public void testDatabaseErrorOnUpdateConfigurationReturnsError() throws Exception
	{
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");
		
		HashMap<String, String> nodeProperties = new HashMap<String, String>();
		nodeProperties.put(Affiliation.OWNER, "juliet@shakespeare.lit");
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		
		Mockito
		    .when(dataStoreMock.nodeExists("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(true);
		Mockito
            .when(dataStoreMock.getNodeConf("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(nodeProperties);
		Mockito
		    .when(dataStoreMock.addNodeConf(Mockito.anyString(), Mockito.any(HashMap.class)))
		    .thenThrow(new DataStoreException());

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito
		    .when(helperMock.isValid())
		    .thenReturn(true);

		nodeConfigure.setDataStore(dataStoreMock);
		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.process(element, jid, request,  null);
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
	    PacketError error = response.getError();
	    assertNotNull(error);
	    assertEquals(PacketError.Type.cancel, error.getType());
	    assertEquals(PacketError.Condition.internal_server_error, error.getCondition());		
	}
	
	@Test
	public void testSuccessfulSettingOfConfigurationReturnsConfirmationStanza() throws Exception
	{
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");
		
		HashMap<String, String> nodeProperties = new HashMap<String, String>();
		nodeProperties.put(Affiliation.OWNER, "juliet@shakespeare.lit");
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		
		Mockito
		    .when(dataStoreMock.nodeExists("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(true);
		Mockito
            .when(dataStoreMock.getNodeConf("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(nodeProperties);
		Mockito
		    .when(dataStoreMock.addNodeConf(Mockito.anyString(), Mockito.any(HashMap.class)))
		    .thenThrow(new DataStoreException());

		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito
		    .when(helperMock.isValid())
		    .thenReturn(true);

		nodeConfigure.setDataStore(dataStoreMock);
		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.process(element, jid, request,  null);
		
		IQ response = (IQ) queue.poll(100, TimeUnit.MILLISECONDS);
	    assertEquals(IQ.Type.result.toString(), response.getType().toString());
	}
	
	@Test
	public void testSettingConfigurationUpdatesSubscribers() throws Exception
	{
		Element element = new BaseElement("configure");
		element.addAttribute("node", "/user/juliet@shakespeare.lit/posts");
		
		HashMap<String, String> nodeProperties = new HashMap<String, String>();
		nodeProperties.put(Affiliation.OWNER, "juliet@shakespeare.lit");
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		
		Mockito
		    .when(dataStoreMock.nodeExists("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(true);
		Mockito
            .when(dataStoreMock.getNodeConf("/user/juliet@shakespeare.lit/posts"))
		    .thenReturn(nodeProperties);

		List<NodeSubscriptionMock> subscribers = new ArrayList<NodeSubscriptionMock>();
		subscribers.add(new NodeSubscriptionMock("romeo@shakespeare.lit"));
		subscribers.add(new NodeSubscriptionMock("hamlet@shakespeare.lit"));
		subscribers.add(new NodeSubscriptionMock("bottom@shakespeare.lit"));

		Mockito
		    .doReturn((Iterator<? extends NodeSubscription>) subscribers.iterator())
            .when(dataStoreMock).getNodeSubscribers(Mockito.anyString());
		
		HelperMock helperMock = Mockito.mock(HelperMock.class);
		Mockito
		    .when(helperMock.isValid())
		    .thenReturn(true);

		nodeConfigure.setDataStore(dataStoreMock);
		nodeConfigure.setConfigurationHelper(helperMock);
		nodeConfigure.process(element, jid, request,  null);
		
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
	    assertEquals(3, queue.size());
	    Packet notification = queue.poll(100, TimeUnit.MILLISECONDS);
	    assertEquals("romeo@shakespeare.lit", notification.getTo().toString());
	    notification = queue.poll(100, TimeUnit.MILLISECONDS);
	    assertEquals("hamlet@shakespeare.lit", notification.getTo().toString());
	    notification = queue.poll(100, TimeUnit.MILLISECONDS);
	    assertEquals("bottom@shakespeare.lit", notification.getTo().toString());
	}
}