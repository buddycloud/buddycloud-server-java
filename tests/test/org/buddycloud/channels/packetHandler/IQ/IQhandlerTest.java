package test.org.buddycloud.channels.packetHandler.IQ;

import java.util.Map;

import junit.framework.TestCase;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packetHandler.IQ.IQHandler;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoInfo;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoItems;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsub;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberRegister;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.State;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.junit.After;
import org.junit.Before;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

import redis.clients.jedis.Jedis;

public class IQhandlerTest extends TestCase {

	public OutQueue outQueue;
	public ErrorQueue errorQueue;
	public Jedis jedis;
	
	@Before
	public void setUp() throws Exception {
		this.outQueue = new OutQueue(null, null, false);
		this.errorQueue = new ErrorQueue(outQueue);
		this.jedis = new Jedis("localhost", 9876);
		
		this.jedis.flushDB();
	}

	@After
	public void tearDown() throws Exception {
		this.jedis.disconnect();
	}

	/**
	 * Let's check that IQHandler returns correctly "Feature-not-implemented
	 * in case of unknown namespace.
	 *  
	 * @throws InterruptedException
	 */
	public void testReturnsFeatureNotImplementedWithUnknownNamespaces() throws InterruptedException {
		
		IQHandler iqHandler = new IQHandler(outQueue, errorQueue, jedis);
		
		String id = "testReturnsFeatureNotImplementedWithUnknownNamespaces";
		
		IQ mockIQ = new IQ(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com");
		iqHandler.ingestPacket(mockIQ);
		
		Thread.sleep(50);
		
		IQ errorIq = (IQ)iqHandler.outQueue.getQueue().poll();
		//System.out.println(errorIq.toXML());
		
		assertEquals(IQ.Type.error, errorIq.getType());
		assertEquals(id, errorIq.getID());
		
		assertEquals("<error type=\"cancel\"><feature-not-implemented xmlns=\"urn:ietf:params:xml:ns:xmpp-stanzas\"/></error>",
					 errorIq.getError().toXML());
		
		//System.out.println(errorIq.getError().toXML());
		
		assertNull(iqHandler.outQueue.getQueue().poll());
		
	}
	
	/**
	 * Let's just check that the register method is successfully called.
	 * If it is, a status channel should be created for tuomas@koski.com.
	 * 
	 * @throws InterruptedException
	 */
	public void testSuccessfullyRoutesJabberRegister() throws InterruptedException {
		
		IQHandler iqHandler = new IQHandler(outQueue, errorQueue, jedis);
		
		String id = "testSuccessfullyRoutesJabberRegister";
		
		IQ mockIQ = new IQ(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com");
		
		mockIQ.setChildElement("query", JabberRegister.NAMESPACE_URI);
		
		iqHandler.ingestPacket(mockIQ);
		
		Thread.sleep(50);
		
		IQ result = (IQ)iqHandler.outQueue.getQueue().poll();
		
		assertEquals(IQ.Type.result, result.getType());
		assertEquals(id, result.getID());
		
		String type = jedis.type("node:/user/tuomas@koski.com/status:conf");
		assertEquals("hash", type);
	}
	
	/**
	 * Let's just check if the correct handler is called. Should return an error since there is no handler 
	 * for the child element "nonexisting".
	 * @throws InterruptedException
	 */
	public void testSuccessfullyRoutesJabberPubsub() throws InterruptedException {
		
		IQHandler iqHandler = new IQHandler(outQueue, errorQueue, jedis);
		
		String id = "testSuccessfullyRoutesJabberPubsub";
		
		IQ mockIQ = new IQ(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com");
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("nonexisting", "http://jabber.org/protocol/pubsub");
		
		iqHandler.ingestPacket(mockIQ);
		
		Thread.sleep(50);
		
		IQ result = (IQ)iqHandler.outQueue.getQueue().poll();
		
		assertEquals(IQ.Type.error, result.getType());
		assertEquals(id, result.getID());
		
		assertEquals("<error type=\"cancel\"><feature-not-implemented xmlns=\"urn:ietf:params:xml:ns:xmpp-stanzas\"/><unsupported xmlns=\"http://jabber.org/protocol/pubsub#errors\" feature=\"nonexisting\"/></error>",
				     result.getError().toXML());
		
	}
	
	public void testSubscribeToExternalNodeWithDiscoverySuccess() throws InterruptedException {
		
		IQHandler iqHandler = new IQHandler(outQueue, errorQueue, jedis);
		
		String id = "testSubscribeToExternalNodeWithDiscoverySuccess";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/nelly@heriveau.fr/status")
		      .addAttribute("jid", mockIQ.getFrom().toString());
		
		/**
		 * We start by sending the subscribe packet from the Client to home channels server.
		 * 
         * <iq type="set" 
         *     id="testSubscribeToExternalNodeWithDiscoverySuccess" 
         *     to="channels.koski.com" 
         *     from="tuomas@koski.com/client">
         *    <pubsub xmlns="http://jabber.org/protocol/pubsub">
         *       <subscribe node="/user/nelly@heriveau.fr/status" 
         *                  jid="tuomas@koski.com/client"/>
         *    </pubsub>
         * </iq>
		 */
		
		iqHandler.ingestPacket(mockIQ.createCopy());
		
		
		Thread.sleep(50);
		
		
		IQ result = (IQ)iqHandler.outQueue.getQueue().poll();
		result.setFrom("channels.koski.com");
		/**
		 * Next in the workflow our channel server will send disco-items backage to heriveau.fr.
		 * 
		 * So in our outqueue we'll have something similar to this:
		 * 
		 * <iq type="get" 
		 *     id="66a2392c-8783-477c-8fd0-c2b0217cee71" 
		 *     from="channels.koski.com" 
		 *     to="heriveau.fr">
		 *    <query xmlns="http://jabber.org/protocol/disco#items"/>
		 * </iq>
		 */

		assertEquals(IQ.Type.get, result.getType());
		assertEquals(mockIQ.getTo(), result.getFrom());
		assertEquals("heriveau.fr", result.getTo().toString());
		
		Element query = new DOMElement("query", new org.dom4j.Namespace("", "http://jabber.org/protocol/disco#items"));
		
		assertEquals(query.asXML(), result.getChildElement().asXML());
		
		// Let's check that our store is in state we want it to be.
		Map<String, String> store = jedis.hgetAll("store:" + result.getID());
		assertEquals(id, store.get("id"));
		assertEquals("/user/nelly@heriveau.fr/status", store.get("node"));
		assertEquals("tuomas@koski.com/client", store.get("jid"));
		assertEquals(State.STATE_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT, store.get(State.KEY_STATE));
		
		
		/**
		 *  We will expect to get back something like this from the heriveau.fr:
		 *  
		 *  <iq type="result" 
		 *      id="ee0c0f97-0e8e-4309-b619-fc0599f3d1be" 
		 *      from="channels.koski.com" to="heriveau.fr">
		 *     <query xmlns="http://jabber.org/protocol/disco#items"/>
		 *        <item jid='bc.hervieau.fr'/>
    	 *     </query>
		 *  </iq>
		 */
		
		IQ discoItemsReply = IQ.createResultIQ(result);
		discoItemsReply.setTo("channels.koski.com");
		query = discoItemsReply.setChildElement("query", JabberDiscoItems.NAMESPACE_URI);
		query.addElement("item")
		     .addAttribute("jid", "bc.heriveau.fr");
	
		iqHandler.ingestPacket(discoItemsReply.createCopy());
		
		
		Thread.sleep(50);
		
		
		result = (IQ)iqHandler.outQueue.getQueue().poll();
		
		/**
		 * Now we know that we have a component bc.heriveau.fr. Next we suppose to send it a disco#info
		 * To find out if it's a buddycloud channel.
		 * 
		 * So in our outqueue we'll have something similar to this:
		 * 
		 * <iq type="get" 
		 *     id="ad17c4c4-1acf-4339-9ef8-91b9878f839f" 
		 *     to="bc.heriveau.fr">
		 *    <query xmlns="http://jabber.org/protocol/disco#info"/>
		 * </iq>
		 */
		
		assertEquals(IQ.Type.get, result.getType());
		assertEquals("bc.heriveau.fr", result.getTo().toBareJID());
		
		query = new DOMElement("query", new org.dom4j.Namespace("", JabberDiscoInfo.NAMESPACE_URI));
		
		assertEquals(query.asXML(), result.getChildElement().asXML());
		
		// Let's check that our store is in state we want it to be.
		store = jedis.hgetAll("store:" + result.getID());
		assertEquals(id, store.get("id"));
		assertEquals("/user/nelly@heriveau.fr/status", store.get("node"));
		assertEquals("tuomas@koski.com/client", store.get("jid"));
		assertEquals(State.STATE_DISCO_INFO_TO_COMPONENTS, store.get(State.KEY_STATE));
		
		/**
		 *  We will expect to get back something like this from the bc.heriveau.fr:
		 *  
		 *  <iq type="result" 
		 *      id="bd17933f-0f52-4af8-ace9-0563bfe4b204" 
		 *      from="bc.heriveau.fr">
		 *     <query xmlns="http://jabber.org/protocol/disco#info">
		 *        <identity category="pubsub" type="channels"/>
		 *     </query>
		 *  </iq>
		 */
		
		IQ discoInfoReply = IQ.createResultIQ(result);
		discoInfoReply.setTo("channels.koski.com");
		query = discoInfoReply.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
		query.addElement("identity")
			 .addAttribute("category", "pubsub")
			 .addAttribute("type", "channels");
		
		iqHandler.ingestPacket(discoInfoReply.createCopy());
		
		
		Thread.sleep(50);
		
		
		result = (IQ)iqHandler.outQueue.getQueue().poll();
		
		/**
		 * Now we subscribe.
		 * 
		 * So in our outqueue we'll have something similar to this:
		 * 
		 * <iq type="set" 
		 *     id="4a09d7d7-03bb-49be-96a8-5699e25edcc9" 
		 *     to="bc.heriveau.fr">
		 *    <pubsub xmlns="http://jabber.org/protocol/pubsub" 
		 *            xmlns:bc="http://buddycloud.org/v1">
		 *       <subscribe node="/user/nelly@heriveau.fr/status" 
		 *                  jid="channels.koski.com"/>
         *       <actor xmlns='http://buddycloud.org/v1'>tuomas@koski.com</actor>   
		 *       </pubsub>
		 * </iq>
		 */
		
		assertEquals(IQ.Type.set, result.getType());
		assertEquals("bc.heriveau.fr", result.getTo().toBareJID());
		
		result.setFrom("channels.koski.com");
		
		query = new DOMElement("query", new org.dom4j.Namespace("", JabberDiscoInfo.NAMESPACE_URI));
		pubsub = new DOMElement("pubsub", new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));
		//pubsub.add(new org.dom4j.Namespace("bc", "http://buddycloud.org/v1"));
		Element subscribe = pubsub.addElement("subscribe");
		subscribe.addAttribute("node", store.get("node"))
		         .addAttribute("jid", result.getFrom().toBareJID());
		         //.addAttribute("bc:actor", new JID("tuomas@koski.com").toBareJID());
		pubsub.addElement("actor", "http://buddycloud.org/v1").setText("tuomas@koski.com");
		
		
		assertEquals(pubsub.asXML(), result.getChildElement().asXML());
		
		// Let's check that our store is in state we want it to be.
		store = jedis.hgetAll("store:" + result.getID());
		assertEquals(id, store.get("id"));
		assertEquals("/user/nelly@heriveau.fr/status", store.get("node"));
		assertEquals("tuomas@koski.com/client", store.get("jid"));
		assertEquals(State.STATE_SUBSCRIBE, store.get(State.KEY_STATE));
		
		/**
		 * We suppose to get OK back.
		 * 
		 * <iq type="result" 
		 * 	   id="testRemoteSubscribeSuccess" 
		 *     to="channels.koski.com" 
		 *     from="bc.heriveau.fr">
		 *    <pubsub xmlns="http://jabber.org/protocol/pubsub">
		 *       <subscription node="/user/nelly@heriveau.fr/status" 
		 *                     jid="channels.koski.com" 
		 *                     subscription="unconfigured"/>
		 *    </pubsub>
		 * </iq>";
		 */
		
		IQ subscriptionReply = IQ.createResultIQ(result);
		subscriptionReply.setTo("channels.koski.com");
		
		pubsub = new DOMElement("pubsub", new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));			
		Element subscription = pubsub.addElement("subscription");
		subscription.addAttribute("node", "/user/nelly@heriveau.fr/status");
		subscription.addAttribute("jid", "channels.koski.com");
		subscription.addAttribute("subscription", "unconfigured");
		
		subscriptionReply.setChildElement(pubsub);
		
		iqHandler.ingestPacket(subscriptionReply.createCopy());
		
		
		Thread.sleep(50);
		
		
		result = (IQ)iqHandler.outQueue.getQueue().poll();
		
		/**
		 * Finally our original request should be answered. We should be subscribed. 
		 * Following should be our last reply:
		 * 
		 * <iq type="result" 
		 *     id="testSubscribeToExternalNodeWithDiscoverySuccess" 
		 *     to="tuomas@koski.com/client">
		 *    <pubsub xmlns="http://jabber.org/protocol/pubsub">
		 *       <subscription node="/user/nelly@heriveau.fr/status" 
		 *                     jid="tuomas@koski.com" 
		 *                     subscription="unconfigured"/>
		 *       </pubsub>
		 *    </iq>
		 */
		
		assertEquals(IQ.Type.result, result.getType());
		assertEquals("testSubscribeToExternalNodeWithDiscoverySuccess", result.getID());
		assertEquals("tuomas@koski.com/client", result.getTo().toString());
		
		result.setFrom("channels.koski.com");
		
		pubsub = new DOMElement("pubsub", new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));
		pubsub.addElement("subscription")
		      .addAttribute("node", "/user/nelly@heriveau.fr/status")
		      .addAttribute("jid", "tuomas@koski.com")
		      .addAttribute("subscription", "unconfigured");
		
		assertEquals(pubsub.asXML(), result.getChildElement().asXML());
		
		// Let's check that our store is in state we want it to be.
		store = jedis.hgetAll("store:" + result.getID());
		assertTrue(store.isEmpty());
	
		assertTrue(jedis.sismember("node:/user/nelly@heriveau.fr/status:subscribers", result.getTo().toBareJID()));
		store = jedis.hgetAll("node:/user/nelly@heriveau.fr/status:subscriber:" + result.getTo().toBareJID());
		
		assertEquals("unconfigured", store.get("subscription"));
		assertEquals("publisher", store.get("affiliation"));
		assertEquals("bc.heriveau.fr", store.get("channel-server"));
		
		assertTrue(jedis.sismember(JedisKeys.REMOTE_NODES, "/user/nelly@heriveau.fr/status"));
		assertEquals("bc.heriveau.fr", jedis.get(JedisKeys.REMOTE_NODE + ":/user/nelly@heriveau.fr/status:jid"));
	}
	
}
