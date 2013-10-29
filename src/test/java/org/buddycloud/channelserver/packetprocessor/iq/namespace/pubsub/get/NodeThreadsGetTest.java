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
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class NodeThreadsGetTest extends IQTestHandler {

	private BlockingQueue<Packet> queue;
	private ChannelManager channelManager;
	private NodeThreadsGet threadsGet;
	
	@Before
	public void setUp() {
		this.queue = new LinkedBlockingQueue<Packet>();
		this.channelManager = Mockito.mock(ChannelManager.class);
		this.threadsGet = new NodeThreadsGet(queue, channelManager);
	}
	
	@Test
	public void testPassingThreadsAsElementName() {
		Element element = new BaseElement("threads");
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
		Element threadsEl = request.getChildElement().element("threads");
		threadsGet.process(threadsEl, request.getFrom(), request, null);
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
		Element threadsEl = request.getChildElement().element("threads");
		threadsGet.process(threadsEl, request.getFrom(), request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}
	
	@Test
	public void testUserNotInAuthorizedChannel() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
		Element threadsEl = request.getChildElement().element("threads");
		
		String node = threadsEl.attributeValue("node");
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		
		threadsGet.process(threadsEl, request.getFrom(), request, null);
		Packet response = queue.poll();
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
	}
	
	@Test
	public void testUserOutcastInOpenChannel() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
		Element threadsEl = request.getChildElement().element("threads");
		
		String node = threadsEl.attributeValue("node");
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		
		Map<String, String> conf = new HashMap<String, String>();
		conf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		Mockito.when(channelManager.getNodeConf(node)).thenReturn(conf);
		
		NodeAffiliationImpl affiliation = new NodeAffiliationImpl(node, 
				request.getFrom(), Affiliations.outcast, new Date());
		Mockito.when(channelManager.getUserAffiliation(node, 
				request.getFrom())).thenReturn(affiliation);
		
		NodeSubscriptionImpl subscription = new NodeSubscriptionImpl(node, 
				request.getFrom(), Subscriptions.subscribed, new Date());
		Mockito.when(channelManager.getUserSubscription(node, 
				request.getFrom())).thenReturn(subscription);
		
		threadsGet.process(threadsEl, request.getFrom(), request, null);
		Packet response = queue.poll();
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
	}
	
	@Test
	public void testWrongAfterItemRSM() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-rsm.stanza");
		Element threadsEl = request.getChildElement().element("threads");
		
		String node = threadsEl.attributeValue("node");
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		
		Map<String, String> conf = new HashMap<String, String>();
		conf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		Mockito.when(channelManager.getNodeConf(node)).thenReturn(conf);
		
		Element rsmEl = request.getChildElement().element("set");
		threadsGet.process(threadsEl, request.getFrom(), request, rsmEl);
		Packet response = queue.poll();
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found, error.getCondition());
	}
	
	@Test
	public void testSucessfulEmptyResponse() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
		Element threadsEl = request.getChildElement().element("threads");
		
		String node = threadsEl.attributeValue("node");
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		
		Map<String, String> conf = new HashMap<String, String>();
		conf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		Mockito.when(channelManager.getNodeConf(node)).thenReturn(conf);

		Mockito.when(channelManager.getNodeThreads(Mockito.eq(node), Mockito.anyString(), 
				Mockito.anyInt())).thenReturn(new ResultSetImpl<NodeThread>(
						new LinkedList<NodeThread>()));
		
		threadsGet.process(threadsEl, request.getFrom(), request, null);
		Packet response = queue.poll();
		
		Assert.assertNull(response.getError());
		Assert.assertNull(response.getElement().element("pubsub").element("thread"));
	}
	
	@SuppressWarnings("unchecked")
	@Test
	public void testSucessfulNonEmptyResponse() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/threads/request-with-node.stanza");
		Element threadsEl = request.getChildElement().element("threads");
		
		String node = threadsEl.attributeValue("node");
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		
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
		
		threadsGet.process(threadsEl, request.getFrom(), request, null);
		Packet response = queue.poll();
		
		Assert.assertNull(response.getError());
		Element responsePubsubEl = response.getElement().element("pubsub");
		
		List<Element> responseThreadsEl = responsePubsubEl.elements("thread");
		Assert.assertNotNull(responseThreadsEl);
		Assert.assertEquals(2, responseThreadsEl.size());
		
		Element responseRsmEl = responsePubsubEl.element("set");
		Assert.assertEquals("itemA", responseRsmEl.elementText("first"));
		Assert.assertEquals("itemC", responseRsmEl.elementText("last"));
		Assert.assertEquals("2", responseRsmEl.elementText("count"));
	}
}
