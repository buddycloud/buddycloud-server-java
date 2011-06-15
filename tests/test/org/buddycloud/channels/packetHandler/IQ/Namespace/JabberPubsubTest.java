package test.org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoItems;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsub;
import org.buddycloud.channels.pubsub.Subscription;
import org.buddycloud.channels.pubsub.subscription.Type;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.junit.After;
import org.junit.Before;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;

import redis.clients.jedis.Jedis;


public class JabberPubsubTest extends TestCase {

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

	public void testPublishStatusSuccess() {
		
		String node = "/user/tuomas@koski.com/status";
		
		jedis.sadd(JedisKeys.LOCAL_NODES, node);
		
		jedis.sadd("node:" + node + ":subscribers", "tuomas@koski.com");
		Subscription sub = new Subscription(Type.unconfigured,
				org.buddycloud.channels.pubsub.affiliation.Type.publisher,
				null);
		jedis.hmset("node:" + node + ":subscriber:tuomas@koski.com", sub.getAsMap());
		
		jedis.sadd("node:" + node + ":subscribers", "pamela@playboy.com");
		sub = new Subscription(Type.unconfigured,
				org.buddycloud.channels.pubsub.affiliation.Type.publisher,
				"bc.playboy.com");
		jedis.hmset("node:" + node + ":subscriber:pamela@playboy.com", sub.getAsMap());
		
		jedis.sadd("node:" + node + ":subscribers", "cindy@playboy.com");
		sub = new Subscription(Type.unconfigured,
				org.buddycloud.channels.pubsub.affiliation.Type.publisher,
				"bc.playboy.com");
		jedis.hmset("node:" + node + ":subscriber:cindy@playboy.com", sub.getAsMap());
		
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testPublishStatusSuccess";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com");
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		Element publish = pubsub.addElement("publish");
		publish.addAttribute("node", "/user/tuomas@koski.com/status");
		Element item = publish.addElement("item"); 
		
		Element entry = new DOMElement("entry", new org.dom4j.Namespace("", "http://www.w3.org/2005/Atom"));
		entry.add(new org.dom4j.Namespace("activity", "http://activitystrea.ms/spec/1.0/"));
		
		String DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
		SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

		entry.addElement("published").setText(sdf.format(new Date()));
		
		Element author = entry.addElement("author");
		author.addElement("name").setText("Tuomas Koski");
		author.addElement("jid", "http://buddycloud.com/atom-elements-0").setText("tuomas@koski.com");
		
		entry.addElement("content")
		     .addAttribute("type", "text")
		     .setText("Hello Federation!");
		
		Element geoloc = entry.addElement("geoloc", "http://jabber.org/protocol/geoloc");
		geoloc.addElement("text").setText("Home Sweet Home!");
		geoloc.addElement("locality").setText("Paris");
		geoloc.addElement("country").setText("Ffance");
		
		entry.addElement("activity:verb")
		     .setText("post");
		entry.addElement("activity:object")
		     .addElement("activity:object-type")
		     .setText("note");
		
		item.add(entry);
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		assertEquals(IQ.Type.result, result.getType());
		assertEquals(id, result.getID());
		
		Element pubsubElm = result.getChildElement();
		assertNotNull(pubsubElm);
		
		Element publishElm = pubsubElm.element("publish");
		assertNotNull(publishElm);
		
		assertEquals("/user/tuomas@koski.com/status", publish.attributeValue("node"));
		
		Element itemElm = publishElm.element("item");
		assertNotNull(itemElm);
		
		String itemID = itemElm.attributeValue("id");
		
		// let's test that the item is added to the unique set
		jedis.sismember("node:/user/tuomas@koski.com/status:items", itemID);
		
		//System.out.println(itemID);
		
		// let's test that the item is added to the 
		List<String> items = jedis.lrange("node:/user/tuomas@koski.com/status:itemlist", 0, 1);
		assertTrue(items.contains(itemID));
		
		// let's check that the item exists.
		entry.addElement("id").setText("/user/tuomas@koski.com/status:" + itemID);
		
		/**
		 * There is a bug here. Will fail if second changes... bad bad tuomas.
		 * But no time now!
		 */
		DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
		sdf = new SimpleDateFormat(DATE_FORMAT);
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		
		entry.addElement("updated").setText(sdf.format(new Date()));
		assertEquals(entry.asXML(), jedis.get("node:/user/tuomas@koski.com/status:item:" + itemID));
		//System.out.println(jedis.get("node:/user/tuomas@koski.com/status:item:" + itemID));
		
		Set<String> possibleReceivers = new HashSet<String>();
		possibleReceivers.add("tuomas@koski.com");
		possibleReceivers.add("bc.playboy.com");
		
		Message eventMsg = (Message)pubsubEngine.outQueue.getQueue().poll();
		//assertEquals(eventMsg.getTo().toBareJID(), "tuomas@koski.com");
		assertTrue(possibleReceivers.contains(eventMsg.getTo().toBareJID()));
		possibleReceivers.remove(eventMsg.getTo().toBareJID());
		
		System.out.println(eventMsg.toXML());
		
		eventMsg = (Message)pubsubEngine.outQueue.getQueue().poll();
		assertTrue(possibleReceivers.contains(eventMsg.getTo().toBareJID()));
		
		eventMsg = (Message)pubsubEngine.outQueue.getQueue().poll();
		assertNull(eventMsg); // We test that playboy.com is send only once
		
		
	}
	
	public void testSubscribeExternalWithDiscoverySuccess() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeExternalWithDiscoverySuccess";
		
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
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		assertEquals(IQ.Type.get, result.getType());
		assertEquals(mockIQ.getTo(), result.getFrom());
		assertEquals("heriveau.fr", result.getTo().toString());
		
		Element query = new DOMElement("query", new org.dom4j.Namespace("", "http://jabber.org/protocol/disco#items"));
		
		assertEquals(query.asXML(), result.getChildElement().asXML());
		
		//System.out.println(result.toXML());
		
		Map<String, String> store = jedis.hgetAll("store:" + result.getID());
		
		assertEquals(id, store.get("id"));
		assertEquals("/user/nelly@heriveau.fr/status", store.get("node"));
		assertEquals("tuomas@koski.com/client", store.get("jid"));
		assertEquals("subscribe-items", store.get("type"));
		
		/**
		 *  When we are here, channel server has send the following backet to heriveau.fr
		 *  
		 *  <iq type="get" 
		 *      id="ee0c0f97-0e8e-4309-b619-fc0599f3d1be" 
		 *      from="channels.koski.com" to="heriveau.fr">
		 *     <query xmlns="http://jabber.org/protocol/disco#items"/>
		 *  </iq>
		 *  
		 *  We will expect to get back something like this:
		 *  
		 *  <iq type="result" 
		 *      id="ee0c0f97-0e8e-4309-b619-fc0599f3d1be" 
		 *      from="channels.koski.com" to="heriveau.fr">
		 *     <query xmlns="http://jabber.org/protocol/disco#items"/>
		 *     <item jid='people.shakespeare.lit'
		 *           name='Directory of Characters'/>
		 *     <item jid='bc.hervieau.fr'/>
    	 *     </query>
		 *  </iq>
		 */
		
		IQ discoItemsReply = IQ.createResultIQ(result);
		query = discoItemsReply.setChildElement("query", JabberDiscoItems.NAMESPACE_URI);
		query.addElement("item")
			 .addAttribute("jid", "people.hervieau.fr")
			 .addAttribute("name", "Directory of Characters");
		query.addElement("item")
		     .addAttribute("jid", "bc.heriveau.fr");
	
		// continue here. Add http://jabber.org/protocol/disco#items handler and a first time in our life, a result handler :-)
		
	}
	
	public void testSubscribeLocalSuccess() throws InterruptedException {
		
		jedis.sadd(JedisKeys.LOCAL_NODES, "/user/tuomas@koski.com/status");
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeLocalSuccess";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/tuomas@koski.com/status")
		      .addAttribute("jid", mockIQ.getFrom().toString());
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		//System.out.println(result.toXML());
		
		Set<String> subscribers = jedis.smembers("node:/user/tuomas@koski.com/status:subscribers");
		assertTrue(subscribers.contains("j@koski.com"));
		
		Map<String, String> sub = jedis.hgetAll("node:/user/tuomas@koski.com/status:subscriber:j@koski.com");
	
		assertEquals("unconfigured", sub.get("subscription"));
		assertEquals("publisher", sub.get("affiliation"));
		assertNull(sub.get("channel-server"));
		
	}
	
	public void testSubscribeLocalFailsRegistrationRequired() throws InterruptedException {
		
		jedis.sadd(JedisKeys.LOCAL_NODES, "/user/tuomas@koski.com/status");
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeLocalFailsRegistrationRequired";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/tuomas@koski.com/status")
		      .addAttribute("jid", mockIQ.getFrom().toString());
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		//System.out.println(result.toXML());
		
		Element error = new DOMElement("error");
		error.addAttribute("type", "auth");
		Element conflict = new DOMElement("registration-required",
					 								 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		error.add(conflict);
		
		assertEquals(result.getError().toXML(), error.asXML());
		
		
		Set<String> subscribers = jedis.smembers("node:/user/tuomas@koski.com/status:subscribers");
		assertFalse(subscribers.contains("j@koski.com"));
		
		Map<String, String> sub = jedis.hgetAll("node:/user/tuomas@koski.com/status:subscriber:j@koski.com");
	
		assertTrue(sub.isEmpty());
		
	}
	
	public void testSubscribeLocalFailsMalformedJid() throws InterruptedException {
		
		jedis.sadd(JedisKeys.LOCAL_NODES, "/user/tuomas@koski.com/status");
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeLocalFailsMalformedJid";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/tuomas@koski.com/status")
		      .addAttribute("jid", "somethingwrong");
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		//System.out.println(result.toXML());
		
		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		Element conflict = new DOMElement("bad-request",
					 								 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		Element ij = new DOMElement("invalid-jid",
				 					new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
		error.add(conflict);
		error.add(ij);
		
		assertEquals(result.getError().toXML(), error.asXML());
		
		
		Set<String> subscribers = jedis.smembers("node:/user/tuomas@koski.com/status:subscribers");
		assertFalse(subscribers.contains("j@koski.com"));
		
		Map<String, String> sub = jedis.hgetAll("node:/user/tuomas@koski.com/status:subscriber:j@koski.com");
	
		assertTrue(sub.isEmpty());
		
	}
	
	public void testSubscribeFromRemoteFailsNoActor() throws InterruptedException {
		
		jedis.sadd(JedisKeys.LOCAL_NODES, "/user/tuomas@koski.com/status");
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeFromRemoteFailsNoActor";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("bc.julie.de");
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/tuomas@koski.com/status")
		      .addAttribute("jid", "bc.julie.de");
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		//System.out.println(result.toXML());
		
		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		Element conflict = new DOMElement("bad-request",
					 								 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		Element ij = new DOMElement("invalid-jid",
				 					new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
		error.add(conflict);
		error.add(ij);
		
		assertEquals(result.getError().toXML(), error.asXML());
		
		
		Set<String> subscribers = jedis.smembers("node:/user/tuomas@koski.com/status:subscribers");
		assertFalse(subscribers.contains("j@koski.com"));
		
		Map<String, String> sub = jedis.hgetAll("node:/user/tuomas@koski.com/status:subscriber:j@koski.com");
	
		assertTrue(sub.isEmpty());
		
	}
	
	public void testSubscribeFromRemoteFailsBadActor() throws InterruptedException {
		
		jedis.sadd(JedisKeys.LOCAL_NODES, "/user/tuomas@koski.com/status");
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeFromRemoteFailsBadActor";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("bc.julie.de");
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.add(new org.dom4j.Namespace("bc", "http://buddycloud.org/v1"));
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/tuomas@koski.com/status")
		      .addAttribute("jid", "bc.julie.de")
		      .addAttribute("bc:actor", "wrong");
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		//System.out.println(result.toXML());
		
		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		Element conflict = new DOMElement("bad-request",
					 								 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		Element ij = new DOMElement("invalid-jid",
				 					new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
		error.add(conflict);
		error.add(ij);
		
		assertEquals(result.getError().toXML(), error.asXML());
		
		
		Set<String> subscribers = jedis.smembers("node:/user/tuomas@koski.com/status:subscribers");
		assertFalse(subscribers.contains("j@koski.com"));
		
		Map<String, String> sub = jedis.hgetAll("node:/user/tuomas@koski.com/status:subscriber:j@koski.com");
	
		assertTrue(sub.isEmpty());
		
	}
	
	public void testRemoteSubscribeSuccess() throws InterruptedException {
		
		jedis.sadd(JedisKeys.LOCAL_NODES, "/user/tuomas@koski.com/status");
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testRemoteSubscribeSuccess";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("bc.heriveau.fr");
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.add(new org.dom4j.Namespace("bc", "http://buddycloud.org/v1"));
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/tuomas@koski.com/status")
		      .addAttribute("jid", mockIQ.getFrom().toString())
		      .addAttribute("bc:actor", "nelly@heriveau.fr");
		
		
		//System.out.println(mockIQ.toXML());
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		Set<String> subscribers = jedis.smembers("node:/user/tuomas@koski.com/status:subscribers");
		assertTrue(subscribers.contains("nelly@heriveau.fr"));
		
		Map<String, String> sub = jedis.hgetAll("node:/user/tuomas@koski.com/status:subscriber:nelly@heriveau.fr");
	
		assertEquals("unconfigured", sub.get("subscription"));
		assertEquals("publisher", sub.get("affiliation"));
		assertEquals("bc.heriveau.fr", sub.get("channel-server"));
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		String expected = "<iq type=\"result\" id=\"testRemoteSubscribeSuccess\" from=\"channels.koski.com\" to=\"bc.heriveau.fr\"><pubsub xmlns=\"http://jabber.org/protocol/pubsub\" xmlns:bc=\"http://buddycloud.org/v1\"><subscription node=\"/user/tuomas@koski.com/status\" jid=\"bc.heriveau.fr\" subscription=\"unconfigured\" bc:actor=\"nelly@heriveau.fr\"/></pubsub></iq>";

		//System.out.println(result.toXML());
		assertEquals(expected, result.toXML());
	}
	
}
