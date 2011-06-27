package test.org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsub;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsubOwner;
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


public class JabberPubsubOwnerTest extends TestCase {

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

	public void testConfigure() throws InterruptedException {
		
		String id = "testConfigure";
		String node = "my_little_paris@koski.com";
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
		
		Map <String, String> conf = new HashMap<String, String>();
		conf.put("pubsub#type", "http://www.w3.org/2005/Atom");
		conf.put("pubsub#title", "My little paris.");
		conf.put("pubsub#description", "Description here.");
		conf.put("pubsub#publish_model", "publishers");
		conf.put("pubsub#access_model", "open");
		conf.put("pubsub#creation_date", "2011-07-27T13:21:00Z");
		conf.put("pubsub#owner", "tuomas@koski.com");
		conf.put("pubsub#default_affiliation", org.buddycloud.channels.pubsub.affiliation.Type.member.toString());
		conf.put("pubsub#num_subscribers", "1");
		conf.put("pubsub#notify_config", "1");
		
		jedis.hmset("node:" + node + ":conf", conf);
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.set);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com/client");
		
		// We add user as registered
		jedis.sadd(JedisKeys.LOCAL_USERS, mockIQ.getFrom().toBareJID());
		
		Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsubOwner.NAMESPACE_URI);
		Element configure = pubsub.addElement("configure")
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
		
        configure.add(x);
        
        JabberPubsubOwner pubsubEngine = new JabberPubsubOwner(this.outQueue, this.errorQueue, this.jedis);
        
        pubsubEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ result = (IQ)pubsubEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		assertEquals(IQ.Type.result, result.getType());
		
		conf = jedis.hgetAll("node:" + node + ":conf");
		assertEquals("http://www.w3.org/2005/Atom", conf.get("pubsub#type"));
		assertEquals("The Awesome Title!", conf.get("pubsub#title"));
		assertEquals("The Awesome Description!", conf.get("pubsub#description"));
		assertEquals("publishers", conf.get("pubsub#publish_model"));
		assertEquals("open", conf.get("pubsub#access_model"));
		assertEquals(mockIQ.getFrom().toBareJID(), conf.get("pubsub#owner"));
		assertEquals(org.buddycloud.channels.pubsub.affiliation.Type.member.toString(), conf.get("pubsub#default_affiliation"));
        
		Set<String> possibleReceivers = new HashSet<String>();
		possibleReceivers.add("tuomas@koski.com");
		possibleReceivers.add("bc.playboy.com");
		
		Message eventMsg = (Message)pubsubEngine.outQueue.getQueue().poll();

		assertTrue(possibleReceivers.contains(eventMsg.getTo().toBareJID()));
		possibleReceivers.remove(eventMsg.getTo().toBareJID());
				
		eventMsg = (Message)pubsubEngine.outQueue.getQueue().poll();
		assertTrue(possibleReceivers.contains(eventMsg.getTo().toBareJID()));
		
		System.out.println(eventMsg.toXML());
		
		eventMsg = (Message)pubsubEngine.outQueue.getQueue().poll();
		assertNull(eventMsg); // We test that playboy.com is send only once
		
		// TODO, add here the verification of the event.
	}

}
