package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
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

public class ItemsGetTest extends IQTestHandler {

	private IQ request;
	private ItemsGet itemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private Mock dataStore = new Mock();

	@Before
	public void setUp() throws Exception {

		queue    = new LinkedBlockingQueue<Packet>();
		itemsGet = new ItemsGet(queue, dataStore);
		request  = readStanzaAsIq("/iq/pubsub/affiliation/affiliationChange.stanza");
		element  = new BaseElement("items");
	}
	
	@Test
	public void testPassingAffiliationsAsElementNameReturnsTrue() {
		assertTrue(itemsGet.accept(element));
	}

	@Test
	public void testPassingNotCreateAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		assertFalse(itemsGet.accept(element));
	}
	
	@Test
	public void testMissingNodeAttributeReturnsErrorStanza() throws Exception
	{
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}
	
	@Test
	public void testNodeWhichDoesntExistReturnsNotFoundStanza() throws Exception
	{
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenReturn(false);
		
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.modify, error.getType());
		assertEquals("nodeid-required", error.getApplicationConditionName());
	}
	
	@Test
	public void testDataStoreExceptionReturnsInternalServerErrorStanza() throws Exception
	{
		element.addAttribute("node", node);
		DataStore dataStoreMock = Mockito.mock(Mock.class);
		Mockito.when(dataStoreMock.nodeExists(node)).thenThrow(
				DataStoreException.class);
		itemsGet.setDataStore(dataStoreMock);
		
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

		PacketError error = response.getError();
		assertNotNull(error);
		assertEquals(PacketError.Type.wait, error.getType());
		assertEquals(PacketError.Condition.internal_server_error, error.getCondition());
	}
	
	@Test
	@Ignore("not ready yet")
	public void testUnsubscribedUserToPrivateChannelCanNotViewItems() throws Exception
	{

	}
	
	@Test
	@Ignore("not ready yet")
	public void testBannedUsersCanNotRetrieveItems() throws Exception
	{
		
	}
	
	@Test
	@Ignore("not ready yet")
	public void testRequestingSubscriptionsNodeReturnsSubscriptionsItems() throws Exception
	{
		
	}
	
	@Test
	@Ignore("not ready yet")
	public void test()
	{
		
	}
}