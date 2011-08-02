package test.org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsub;
import org.buddycloud.channels.pubsub.Subscription;
import org.buddycloud.channels.pubsub.subscription.Type;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.State;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.junit.After;
import org.junit.Before;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;
import org.xmpp.packet.PacketError;

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
		entry.addElement("id").setText("tag:channels.koski.com,/user/tuomas@koski.com/status," + itemID);
		
		/**
		 * There is a bug here. Will fail if second changes... bad bad tuomas.
		 * But no time now!
		 */
		DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
		sdf = new SimpleDateFormat(DATE_FORMAT);
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		
		//System.out.println(jedis.get("node:/user/tuomas@koski.com/status:item:" + itemID));
		
		
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
		
		//System.out.println(eventMsg.toXML());
		
		eventMsg = (Message)pubsubEngine.outQueue.getQueue().poll();
		assertTrue(possibleReceivers.contains(eventMsg.getTo().toBareJID()));
		
		eventMsg = (Message)pubsubEngine.outQueue.getQueue().poll();
		assertNull(eventMsg); // We test that playboy.com is send only once
		
	}
	
	public void testSubscribeExternalStartsDiscoverySuccess() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeExternalStartsDiscoverySuccess";
		
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
		result.setFrom("channels.koski.com");
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
		assertEquals(State.STATE_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT, store.get(State.KEY_STATE));
		
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
		assertEquals(IQ.Type.result, result.getType());
		
		
		Set<String> subscribers = jedis.smembers("node:/user/tuomas@koski.com/status:subscribers");
		assertTrue(subscribers.contains("j@koski.com"));
		
		Map<String, String> sub = jedis.hgetAll("node:/user/tuomas@koski.com/status:subscriber:j@koski.com");
	
		assertEquals("unconfigured", sub.get("subscription"));
		assertEquals("publisher", sub.get("affiliation"));
		assertNull(sub.get("channel-server"));
		
	}
	
//	public void testSubscribeLocalFailsRegistrationRequired() throws InterruptedException {
//		
//		jedis.sadd(JedisKeys.LOCAL_NODES, "/user/tuomas@koski.com/status");
//		
//		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
//		
//		String id = "testSubscribeLocalFailsRegistrationRequired";
//		
//		IQ mockIQ = new IQ();
//		mockIQ.setType(IQ.Type.set);
//		mockIQ.setID(id);
//		mockIQ.setTo("channels.koski.com");
//		mockIQ.setFrom("j@koski.com/client");
//		
//		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
//		pubsub.addElement("subscribe")
//		      .addAttribute("node", "/user/tuomas@koski.com/status")
//		      .addAttribute("jid", mockIQ.getFrom().toString());
//		
//		pubsubEngine.ingestPacket(mockIQ.createCopy());
//		
//		Thread.sleep(50);
//		
//		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
//		//System.out.println(result.toXML());
//		
//		Element error = new DOMElement("error");
//		error.addAttribute("type", "auth");
//		Element conflict = new DOMElement("registration-required",
//					 								 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
//		error.add(conflict);
//		
//		assertEquals(result.getError().toXML(), error.asXML());
//		
//		
//		Set<String> subscribers = jedis.smembers("node:/user/tuomas@koski.com/status:subscribers");
//		assertFalse(subscribers.contains("j@koski.com"));
//		
//		Map<String, String> sub = jedis.hgetAll("node:/user/tuomas@koski.com/status:subscriber:j@koski.com");
//	
//		assertTrue(sub.isEmpty());
//		
//	}
	
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
		//pubsub.add(new org.dom4j.Namespace("bc", "http://buddycloud.org/v1"));
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/tuomas@koski.com/status")
		      .addAttribute("jid", mockIQ.getFrom().toString());
		      //.addAttribute("bc:actor", "nelly@heriveau.fr");
		pubsub.addElement("actor", "http://buddycloud.org/v1").setText("nelly@heriveau.fr");
		
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
	
	public void testCreateWithoutConfSuccess() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testCreateWithoutConfSuccess";
		String node = "status_of_the_development";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("create")
		      .addAttribute("node", node);
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		String type = jedis.type("node:" + node + ":conf");
		assertEquals("hash", type);
		
		Map<String, String> conf = jedis.hgetAll("node:" + node + ":conf");
		assertEquals("http://www.w3.org/2005/Atom", conf.get("pubsub#type"));
		assertEquals("", conf.get("pubsub#title"));
		assertEquals("", conf.get("pubsub#description"));
		assertEquals("publishers", conf.get("pubsub#publish_model"));
		assertEquals("open", conf.get("pubsub#access_model"));
		assertEquals(mockIQ.getFrom().toBareJID(), conf.get("pubsub#owner"));
		assertEquals(org.buddycloud.channels.pubsub.affiliation.Type.publisher.toString(), conf.get("pubsub#default_affiliation"));
		assertEquals("1", conf.get("pubsub#num_subscribers"));
	}
	
	public void testCreateWithConfSuccess() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testCreateWithoutConfSuccess";
		String node = "status_of_the_development";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("create")
		      .addAttribute("node", node);
		
		Element x = new DOMElement("x", new org.dom4j.Namespace("", "jabber:x:data"));
		x.addAttribute("type", "submit");
		Element field = x.addElement("field");
		field.addAttribute("var", "FORM_TYPE")
			 .addAttribute("type", "hidden")
			 .addElement("value").setText("http://jabber.org/protocol/pubsub#node_config");
		
		field = x.addElement("field");
		field.addAttribute("var", "pubsub#title")
			 .addElement("value").setText("The Awesome Title!");
		
		field = x.addElement("field");
		field.addAttribute("var", "pubsub#description")
			 .addElement("value").setText("The Awesome Description!");
		
		field = x.addElement("field");
		field.addAttribute("var", "pubsub#type")
			 .addElement("value").setText("Something that should not be set!");
		
        pubsub.addElement("configure").add(x);
		
        //System.out.println(mockIQ.toXML());
        
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		assertEquals(IQ.Type.result, result.getType());
		
		String type = jedis.type("node:" + node + ":conf");
		assertEquals("hash", type);
		
		Map<String, String> conf = jedis.hgetAll("node:" + node + ":conf");
		assertEquals("http://www.w3.org/2005/Atom", conf.get("pubsub#type"));
		assertEquals("The Awesome Title!", conf.get("pubsub#title"));
		assertEquals("The Awesome Description!", conf.get("pubsub#description"));
		assertEquals("publishers", conf.get("pubsub#publish_model"));
		assertEquals("open", conf.get("pubsub#access_model"));
		assertEquals(mockIQ.getFrom().toBareJID(), conf.get("pubsub#owner"));
		assertEquals(org.buddycloud.channels.pubsub.affiliation.Type.publisher.toString(), conf.get("pubsub#default_affiliation"));
		assertEquals("1", conf.get("pubsub#num_subscribers"));
		
		assertTrue(jedis.sismember(JedisKeys.LOCAL_NODES, node));
		
		//jedis.sadd("node:" + node + ":subscribers", reqIQ.getFrom().toBareJID());
		assertTrue(jedis.sismember("node:" + node + ":subscribers", mockIQ.getFrom().toBareJID()));
		
		//jedis.hmset("node:" + node + ":subscriber:" + reqIQ.getFrom().toBareJID(), sub.getAsMap());
		Map<String, String> sub = jedis.hgetAll("node:" + node + ":subscriber:" + mockIQ.getFrom().toBareJID());
		
		assertEquals("subscribed", sub.get(Subscription.KEY_SUBSCRIPTION));
		assertEquals("owner", sub.get(Subscription.KEY_AFFILIATION));
		assertNull(sub.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER));
		
		assertEquals(mockIQ.getFrom().toBareJID(), jedis.get("node:" + node + ":owner"));
	}
	
	public void testGetAllSubscriptions() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		jedis.sadd("j@koski.com:subs", "beer_and_babes@bc.hoituu.com");
		jedis.hset("node:beer_and_babes@bc.hoituu.com:subscriber:j@koski.com", Subscription.KEY_SUBSCRIPTION, "subscribed");
		jedis.sadd("j@koski.com:subs", "babes_and_boobs@channels.koski.com");
		jedis.hset("node:babes_and_boobs@channels.koski.com:subscriber:j@koski.com", Subscription.KEY_SUBSCRIPTION, "subscribed");
		
		String id = "testGetAllSubscriptions";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.get);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscriptions");
		
		//System.out.println(mockIQ.toXML());
        
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		Element expected = new DOMElement("pubsub",
										  new org.dom4j.Namespace("", "http://jabber.org/protocol/pubsub"));
		Element subscriptions = expected.addElement("subscriptions");
		subscriptions.addElement("subscription")
		             .addAttribute("node", "babes_and_boobs@channels.koski.com")
		             .addAttribute("subscription", "subscribed")
		             .addAttribute("jid", "j@koski.com");
		subscriptions.addElement("subscription")
					 .addAttribute("node", "beer_and_babes@bc.hoituu.com")
					 .addAttribute("subscription", "subscribed")
					 .addAttribute("jid", "j@koski.com");
		
		assertEquals(expected.asXML(), result.getChildElement().asXML());
	}	
	
	public void testGetOneSubscriptions() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		jedis.sadd("j@koski.com:subs", "beer_and_babes@bc.hoituu.com");
		jedis.hset("node:beer_and_babes@bc.hoituu.com:subscriber:j@koski.com", Subscription.KEY_SUBSCRIPTION, "subscribed");
		jedis.sadd("j@koski.com:subs", "babes_and_boobs@channels.koski.com");
		jedis.hset("node:babes_and_boobs@channels.koski.com:subscriber:j@koski.com", Subscription.KEY_SUBSCRIPTION, "subscribed");
		
		String id = "testGetOneSubscriptions";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.get);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscriptions").addAttribute("node", "babes_and_boobs@channels.koski.com");
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		Element expected = new DOMElement("pubsub",
										  new org.dom4j.Namespace("", "http://jabber.org/protocol/pubsub"));
		Element subscriptions = expected.addElement("subscriptions");
		subscriptions.addElement("subscription")
		             .addAttribute("node", "babes_and_boobs@channels.koski.com")
		             .addAttribute("subscription", "subscribed")
		             .addAttribute("jid", "j@koski.com");
		
		assertEquals(expected.asXML(), result.getChildElement().asXML());
	}
	
	public void testGetAllAffiliations() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		jedis.sadd("j@koski.com:subs", "beer_and_babes@bc.hoituu.com");
		jedis.hset("node:beer_and_babes@bc.hoituu.com:subscriber:j@koski.com", Subscription.KEY_AFFILIATION, "owner");
		jedis.sadd("j@koski.com:subs", "babes_and_boobs@channels.koski.com");
		jedis.hset("node:babes_and_boobs@channels.koski.com:subscriber:j@koski.com", Subscription.KEY_AFFILIATION, "publisher");
		
		String id = "testGetAllAffiliations";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.get);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("affiliations");
		
		//System.out.println(mockIQ.toXML());
        
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		Element expected = new DOMElement("pubsub",
										  new org.dom4j.Namespace("", "http://jabber.org/protocol/pubsub"));
		Element subscriptions = expected.addElement("affiliations");
		subscriptions.addElement("affiliation")
		             .addAttribute("node", "babes_and_boobs@channels.koski.com")
		             .addAttribute("affiliation", "publisher");
		subscriptions.addElement("affiliation")
					 .addAttribute("node", "beer_and_babes@bc.hoituu.com")
					 .addAttribute("affiliation", "owner");
		
		assertEquals(expected.asXML(), result.getChildElement().asXML());
	}	
	
	public void testGetOneAffiliation() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		jedis.sadd("j@koski.com:subs", "beer_and_babes@bc.hoituu.com");
		jedis.hset("node:beer_and_babes@bc.hoituu.com:subscriber:j@koski.com", Subscription.KEY_AFFILIATION, "owner");
		jedis.sadd("j@koski.com:subs", "babes_and_boobs@channels.koski.com");
		jedis.hset("node:babes_and_boobs@channels.koski.com:subscriber:j@koski.com", Subscription.KEY_AFFILIATION, "publisher");
		
		String id = "testGetOneAffiliation";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.get);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("affiliations").addAttribute("node", "beer_and_babes@bc.hoituu.com");
		
		
		//System.out.println(mockIQ.toXML());
        
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		assertEquals(IQ.Type.result, result.getType());
		
		//System.out.println(result.toXML());
		
		Element expected = new DOMElement("pubsub",
										  new org.dom4j.Namespace("", "http://jabber.org/protocol/pubsub"));
		Element subscriptions = expected.addElement("affiliations");
		subscriptions.addElement("affiliation")
					 .addAttribute("node", "beer_and_babes@bc.hoituu.com")
					 .addAttribute("affiliation", "owner");
		
		assertEquals(expected.asXML(), result.getChildElement().asXML());
	}	
	
	public void testRetract() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		String node = "beer_and_babes@bc.hoituu.com";
		
		jedis.sadd(JedisKeys.LOCAL_NODES, node);
		
		jedis.sadd("node:" + node + ":subscribers", "tuomas@koski.com");
		Subscription sub = new Subscription(Type.unconfigured,
				org.buddycloud.channels.pubsub.affiliation.Type.publisher,
				null);
		jedis.hmset("node:" + node + ":subscriber:tuomas@koski.com", sub.getAsMap());
		
		String iid = "ae890ac52d0df67ed7cfdf51b644e901";
		jedis.sadd("node:" + node + ":itemset", iid);
		jedis.lpush("node:" + node + ":itemlist", iid);
		jedis.set("node:" + node + ":item:" + iid, "t");
		
		String id = "testRetract";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("j@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		Element retract = pubsub.addElement("retract");
		retract.addAttribute("node", node);
		retract.addElement("item").addAttribute("id", iid);
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		assertEquals(IQ.Type.result, result.getType());
		
		Message notify = (Message)pubsubEngine.outQueue.getQueue().poll();
		
		assertEquals(Message.Type.headline, notify.getType());
		
		//System.out.print(notify.toXML());
		
		Element e = notify.getChildElement("event", "http://jabber.org/protocol/pubsub#event");
		
		Element event = new DOMElement("event",
				  					   new org.dom4j.Namespace("", "http://jabber.org/protocol/pubsub#event"));
		Element items = event.addElement("items");
		items.addAttribute("node", node);
		Element r = items.addElement("retract");
		r.addAttribute("id", iid);
		
		assertEquals(e.asXML(), event.asXML());
		
		assertFalse(jedis.sismember("node:" + node + ":itemset", iid));
		assertTrue(0 == jedis.llen("node:" + node + ":itemlist"));
		assertNull(jedis.get("node:" + node + ":item:" + iid));
	}
	
	public void testPublishToExternalNode() throws InterruptedException {
			
		String node = "/user/pamela@anderson.us/status";
		
		jedis.sadd(JedisKeys.REMOTE_NODES, node);
		
		jedis.sadd("node:" + node + ":subscribers", "tuomas@koski.com");
		Subscription sub = new Subscription(Type.subscribed,
											org.buddycloud.channels.pubsub.affiliation.Type.publisher,
											"channelbunnies.anderson.us");
		jedis.hmset("node:" + node + ":subscriber:tuomas@koski.com", sub.getAsMap());
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testPublishToExternalNode";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com/client");
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		Element publish = pubsub.addElement("publish");
		publish.addAttribute("node", node);
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
		
		Thread.sleep(50);
		
		IQ copy = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(copy.toXML());
		
		assertEquals("channelbunnies.anderson.us", copy.getTo().toBareJID());
		assertEquals(IQ.Type.set, copy.getType());
		String actorValue = copy.getChildElement()
						    	.element("actor")
						    	.getText();
		assertEquals(actorValue, "tuomas@koski.com");
	}
	
	public void testExternalNodeSuccessReply() throws InterruptedException {
		
		/**
		 * We should receive something like this:
		 * <iq type='result'
		 *	    from='channelbunnies.anderson.us'
		 *	    to='channels.koski.com'
		 *	    id='publish1'>
		 *	  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
		 *	    <publish node='/user/pamela@anderson.us/status'>
		 *	      <item id='ae890ac52d0df67-ed7cfdf51b-644e901'/>
		 *	    </publish>
		 *	  </pubsub>
		 *	</iq>
		 */
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testExternalNodeSuccessReply";
		String node = "/user/pamela@anderson.us/status";
		String origID = "123456ssssssss";
		String origFrom = "j@koski.com/client";
		
		Map <String, String> store = new HashMap<String, String>();
		store.put(State.KEY_STATE, State.STATE_PUBLISH);
		store.put("id", origID);
		store.put("jid", origFrom);
		jedis.hmset("store:" + id, store);
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.result);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("channelbunnies.anderson.us");
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		Element publish = pubsub.addElement("publish");
		publish.addAttribute("node", node);
		publish.addElement("item").addAttribute("id", "ae890ac52d0df67-ed7cfdf51b-644e901");


		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();

		System.out.println(result.toXML());
		
		assertEquals(IQ.Type.result, result.getType());
		assertEquals(origID, result.getID());
		
		Element pubsubElm = result.getChildElement();
		assertNotNull(pubsubElm);
		
		Element publishElm = pubsubElm.element("publish");
		assertNotNull(publishElm);
		
		assertEquals(node, publish.attributeValue("node"));
		
		Element itemElm = publishElm.element("item");
		assertNotNull(itemElm);
		
		assertEquals(result.getTo().toString(), origFrom);
	}
	
	public void testSubscribeExternalNormalNodeStartsSubscribeSuccessfully() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeExternalNormalNodeStartsSubscribeSuccessfully";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscribe")
		      .addAttribute("node", "bunnies@channels.playboy.com")
		      .addAttribute("jid", mockIQ.getFrom().toString());
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ subsReq = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(subsReq.toXML());
		
		pubsub = new DOMElement("pubsub", new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));
		pubsub.addElement("subscribe")
		      .addAttribute("node", "bunnies@channels.playboy.com")
		      .addAttribute("jid", "channels.koski.com");
		pubsub.addElement("actor", "http://buddycloud.org/v1").setText("tuomas@koski.com");
		
		assertEquals(null, subsReq.getFrom());
		assertEquals("channels.playboy.com", subsReq.getTo().toBareJID());
		assertEquals(pubsub.asXML(), subsReq.getChildElement().asXML());
		
		//System.out.println(subsReq.toXML());
		
		Map <String, String> store = jedis.hgetAll("store:" + subsReq.getID());
		assertEquals(store.get(State.KEY_STATE), State.STATE_SUBSCRIBE);
		assertEquals(store.get("id"), id);
		assertEquals(store.get("jid"), "tuomas@koski.com/client");
		assertEquals(store.get("node"), "bunnies@channels.playboy.com");
		
	}
	
	public void testSubscribeExternalNormalNodeNotFoundReturnsErrorToUser() throws InterruptedException {
		
		JabberPubsub pubsubEngine = new JabberPubsub(this.outQueue, this.errorQueue, this.jedis);
		
		String id = "testSubscribeExternalNormalNodeNotFoundReturnsErrorToUser";
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscribe")
		      .addAttribute("node", "bunnies@channels.playboy.com")
		      .addAttribute("jid", mockIQ.getFrom().toString());
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ subsReq = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(subsReq.toXML());
		
		pubsub = new DOMElement("pubsub", new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));
		pubsub.addElement("subscribe")
		      .addAttribute("node", "bunnies@channels.playboy.com")
		      .addAttribute("jid", "channels.koski.com");
		pubsub.addElement("actor", "http://buddycloud.org/v1").setText("tuomas@koski.com");
		
		assertEquals(null, subsReq.getFrom());
		assertEquals("channels.playboy.com", subsReq.getTo().toBareJID());
		assertEquals(pubsub.asXML(), subsReq.getChildElement().asXML());
		
		//System.out.println(subsReq.toXML());
		
		Map <String, String> store = jedis.hgetAll("store:" + subsReq.getID());
		assertEquals(store.get(State.KEY_STATE), State.STATE_SUBSCRIBE);
		assertEquals(store.get("id"), id);
		assertEquals(store.get("jid"), "tuomas@koski.com/client");
		assertEquals(store.get("node"), "bunnies@channels.playboy.com");
		
		// We receive and error because remove server not found
		/* <iq type="error" 
		       id="96ec76ce-391e-46b6-a6cc-04b308433619" 
		       to="channels.koski.com" 
		       from="channels.playboy.com">
		      <pubsub xmlns="http://jabber.org/protocol/pubsub">
		         <subscribe node="bunnies@channels.playboy.com" jid="channels.koski.com"/>
		         <actor xmlns="http://buddycloud.org/v1">tuomas@koski.com</actor>
		      </pubsub>
		      <error code="404" type="cancel">
		         <remote-server-not-found xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/>
		      </error>
		   </iq>
		*/
		mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(subsReq.getID());
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com/client");
		
		pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
		pubsub.addElement("subscribe")
		      .addAttribute("node", "bunnies@channels.playboy.com")
		      .addAttribute("jid", "channels.koski.com");
		pubsub.addElement("actor", "http://buddycloud.org/v1").setText("tuomas@koski.com");
		
		Element error = new DOMElement("error");
		error.addAttribute("type", "cancel")
		     .addAttribute("code", "404");
		Element notFound = new DOMElement("remote-server-not-found",
					 					  new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		error.add(notFound);
		PacketError e = new PacketError(error);
		mockIQ.setError(e);
		
		System.out.println(mockIQ.toXML());
		
		pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		subsReq = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		System.out.println(subsReq.toXML());
		
		store = jedis.hgetAll("store:" + subsReq.getID());
	    assertTrue(store.isEmpty());
	    
	    assertEquals(subsReq.getType(), IQ.Type.error);
	    assertEquals("tuomas@koski.com/client", subsReq.getTo().toString());
	}
	
}
