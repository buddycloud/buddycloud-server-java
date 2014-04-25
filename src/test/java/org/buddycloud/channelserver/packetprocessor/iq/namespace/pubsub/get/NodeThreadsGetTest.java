package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeThread;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeThreadImpl;
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

public class NodeThreadsGetTest extends IQTestHandler {

	private BlockingQueue<Packet> queue;
	private ChannelManager channelManager;
	private NodeThreadsGet threadsGet;
	private Element element = new BaseElement("threads");
	
	@Before
	public void setUp() throws NodeStoreException {
		this.queue = new LinkedBlockingQueue<Packet>();
		this.channelManager = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString())).thenReturn(true);
		Mockito.when(channelManager.nodeExists(Mockito.anyString())).thenReturn(true);
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class))).thenReturn(true);
		
		this.threadsGet = new NodeThreadsGet(queue, channelManager);
	}
	
	@Test
	public void testPassingThreadsAsElementName() {
		Assert.assertTrue(threadsGet.accept(element));
	}
	
	@Test
	public void testPassingNoThreadsAsElementName() {
		Element element = new BaseElement("non-threads");
		Assert.assertFalse(threadsGet.accept(element));
	}
	
	@Test
	public void testMissingNodeAttribute() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-no-node.stanza");

		Mockito.when(channelManager.isLocalJID(request.getFrom())).thenReturn(true);
		
		threadsGet.process(element, request.getFrom(), request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required",
				error.getApplicationConditionName());
	}
	
	@Test
	public void testInexistentNode() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
		Element element = request.getChildElement().element("threads");

		Mockito.when(channelManager.nodeExists(Mockito.anyString())).thenReturn(false);
		
		threadsGet.process(element, request.getFrom(), request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}
	
	@Test
	public void testUserNotInAuthorizedChannel() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
		
		threadsGet.process(element, request.getFrom(), request, null);
		Packet response = queue.poll();
		
		PacketError error = response.getError();

		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
	}
	
	@Test
	public void testUserOutcastInOpenChannel() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
			
		Map<String, String> conf = new HashMap<String, String>();
		conf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(conf);
		
		NodeAffiliationImpl affiliation = new NodeAffiliationImpl(Mockito.anyString(), 
				request.getFrom(), Affiliations.outcast, new Date());
		Mockito.when(channelManager.getUserAffiliation(Mockito.anyString(), 
				request.getFrom())).thenReturn(affiliation);
		
		NodeSubscriptionImpl subscription = new NodeSubscriptionImpl(Mockito.anyString(), 
				request.getFrom(), Subscriptions.subscribed, new Date());
		Mockito.when(channelManager.getUserSubscription(Mockito.anyString(), 
				request.getFrom())).thenReturn(subscription);
		
		threadsGet.process(element, request.getFrom(), request, null);
		Packet response = queue.poll();
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
	}
	
	@Test
	public void testWrongAfterItemRSM() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-rsm.stanza");
			
		Map<String, String> conf = new HashMap<String, String>();
		conf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(conf);
		
		Element rsmEl = request.getChildElement().element("set");
		threadsGet.process(element, request.getFrom(), request, rsmEl);
		Packet response = queue.poll();
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}
	
	@Test
	public void testSucessfulEmptyResponse() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
	
		Map<String, String> conf = new HashMap<String, String>();
		conf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(conf);

		Mockito.when(channelManager.getNodeThreads(Mockito.anyString(), Mockito.anyString(), 
				Mockito.anyInt())).thenReturn(new ResultSetImpl<NodeThread>(
						new LinkedList<NodeThread>()));
		
		threadsGet.process(element, request.getFrom(), request, null);
		Packet response = queue.poll();
		
		Assert.assertNull(response.getError());
		Assert.assertNull(response.getElement().element("pubsub").element("thread"));
	}
	
	@SuppressWarnings("unchecked")
	@Test
	public void testSucessfulNonEmptyResponse() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
		
		Element element = request.getChildElement().element("threads");
		String node = element.attributeValue("node");

		
		Map<String, String> conf = new HashMap<String, String>();
		conf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		Mockito.when(channelManager.getNodeConf(node)).thenReturn(conf);

		LinkedList<NodeThread> threads = new LinkedList<NodeThread>();
		NodeThreadImpl threadA = new NodeThreadImpl("itemA", new Date());
		threadA.addItem(new NodeItemImpl(node, "itemA", new Date(), "<payload/>"));
		threadA.addItem(new NodeItemImpl(node, "itemB", new Date(), "<payload/>", "itemA"));
		threads.add(threadA);
		
		NodeThreadImpl threadB = new NodeThreadImpl("itemC", new Date());
		threadB.addItem(new NodeItemImpl(node, "itemC", new Date(), "<payload/>"));
		threadB.addItem(new NodeItemImpl(node, "itemD", new Date(), "<payload/>", "itemC"));
		threads.add(threadB);
		
		Mockito.when(channelManager.getNodeThreads(Mockito.eq(node), Mockito.anyString(), 
				Mockito.anyInt())).thenReturn(new ResultSetImpl<NodeThread>(
						threads));
		Mockito.when(channelManager.countNodeThreads(node)).thenReturn(threads.size());
		
		threadsGet.process(element, request.getFrom(), request, null);
		Packet response = queue.poll();
		
		Assert.assertNull(response.getError());
		Element responsePubsubEl = response.getElement().element("pubsub");
		
		List<Element> responseelement = responsePubsubEl.elements("thread");
		Assert.assertNotNull(responseelement);
		Assert.assertEquals(2, responseelement.size());
		
		Element responseRsmEl = responsePubsubEl.element("set");
		Assert.assertEquals("itemA", responseRsmEl.elementText("first"));
		Assert.assertEquals("itemC", responseRsmEl.elementText("last"));
		Assert.assertEquals("2", responseRsmEl.elementText("count"));
	}
	
	@Test
	public void testRemoteNodeNoError() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");

		Mockito.when(channelManager.isLocalNode(Mockito.anyString())).thenReturn(false);
		Mockito.when(channelManager.isCachedNode(Mockito.anyString())).thenReturn(false);
		JID from = request.getFrom();
		threadsGet.process(element, from, request, null);

		Packet response = queue.poll();

		Assert.assertNull(response.getError());

		Element pubsubResponse = response.getElement().element("pubsub");
		Assert.assertNotNull(pubsubResponse);

		Element threadsResponseEl = pubsubResponse.element("threads");
		Assert.assertNotNull(threadsResponseEl);
		Assert.assertEquals(
				request.getChildElement().element("threads").attributeValue("node"),
				threadsResponseEl.attributeValue("node")
		);

		Element actor = pubsubResponse.element("actor");
		Assert.assertNotNull(actor);
		Assert.assertEquals(actor.getText(), from.toBareJID());
	}
	
	@Test
	public void testRemoteRequest() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
		JID from = request.getFrom();
		Mockito.when(channelManager.isLocalJID(from)).thenReturn(false);
		
		Map<String, String> conf = new HashMap<String, String>();
		conf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(conf);
		
		Mockito.when(channelManager.getNodeThreads(Mockito.anyString(), Mockito.anyString(), 
				Mockito.anyInt())).thenReturn(new ResultSetImpl<NodeThread>(
						new LinkedList<NodeThread>()));
		
		threadsGet.process(element, from, request, null);

		Packet response = queue.poll();
		
		Assert.assertNull(response.getError());
		Assert.assertNotNull(response.getElement().element("pubsub"));
		Assert.assertNotNull(response.getElement().attributeValue(
				"remote-server-discover"));
	}
}
