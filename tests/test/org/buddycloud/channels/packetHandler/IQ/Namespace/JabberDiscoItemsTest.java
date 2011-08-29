package test.org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoInfo;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoItems;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.State;
import org.dom4j.Element;
import org.junit.After;
import org.junit.Before;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;

import redis.clients.jedis.Jedis;


public class JabberDiscoItemsTest extends TestCase {

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

	public void testDiscoItemsResultSuccess() throws InterruptedException {
		
		String id = "testDiscoItemsResultSuccess";
		
		Map <String, String> store = new HashMap<String, String>();
		store.put(State.KEY_STATE, State.STATE_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT);
		store.put("id", "original-subs-id-123456");
		store.put("jid", "tuomas@koski.com/client");
		store.put("node", "/user/nelly@heriveau.fr/status");
		
		jedis.hmset("store:" + id, store);
		
		JabberDiscoItems discoItemsEngine = new JabberDiscoItems(this.outQueue, this.errorQueue, this.jedis);
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.result);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("heriveau.fr");
		
		Element query = mockIQ.setChildElement("query", JabberDiscoItems.NAMESPACE_URI);
		query.addElement("item")
			 .addAttribute("jid", "people.heriveau.fr")
			 .addAttribute("name", "Directory of Characters");
		query.addElement("item")
		     .addAttribute("jid", "bc.heriveau.fr");
		
		discoItemsEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ discoInfoReq = (IQ)discoItemsEngine.outQueue.getQueue().poll();
		//System.out.println(discoInfoReq.toXML());
		
		assertEquals(IQ.Type.get, discoInfoReq.getType());
		
		store = jedis.hgetAll("store:" + discoInfoReq.getID());
		
		assertEquals("original-subs-id-123456", store.get("id"));
		assertEquals("/user/nelly@heriveau.fr/status", store.get("node"));
		assertEquals("tuomas@koski.com/client", store.get("jid"));
		assertEquals(State.STATE_DISCO_INFO_TO_COMPONENTS, store.get(State.KEY_STATE));
		assertEquals("bc.heriveau.fr", store.get(State.KEY_COMPONENTS));
		
		IQ expectedIQ = new IQ();
		expectedIQ.setID(discoInfoReq.getID());
		expectedIQ.setType(Type.get);
		expectedIQ.setTo("people.heriveau.fr");
		expectedIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
		
		assertEquals(expectedIQ.toXML(), discoInfoReq.toXML());
		
	}

	public void testLocalDiscoItems() throws InterruptedException {
	
		String node = "/user/channel1@local.com";
		jedis.sadd(JedisKeys.LOCAL_NODES, node);
		jedis.hset("node:" + node + ":conf", "pubsub#title", "Channel 1 Title");
		node = "/user/channel2@local.com";
		jedis.sadd(JedisKeys.LOCAL_NODES, node);
		jedis.hset("node:" + node + ":conf", "pubsub#title", "Channel 2 Title");
		node = "/user/channel3@local.com";
		jedis.sadd(JedisKeys.LOCAL_NODES, node);
		jedis.hset("node:" + node + ":conf", "pubsub#title", "Channel 3 Title");
	
		JabberDiscoItems discoItemsEngine = new JabberDiscoItems(this.outQueue, this.errorQueue, this.jedis);
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.get);
		mockIQ.setID("testLocalDiscoItemsSuccess");
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com/client");
		
		Element query = mockIQ.setChildElement("query", JabberDiscoItems.NAMESPACE_URI);
		
		discoItemsEngine.ingestPacket(mockIQ);
		
		IQ result = (IQ)discoItemsEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		/*
		  <iq type="result" 
		      id="testLocalDiscoItemsSuccess" 
		      from="channels.koski.com" 
		      to="tuomas@koski.com/client">
		     <query xmlns="http://jabber.org/protocol/disco#items">
		        <item jid="channels.koski.com" node="/user/channel1@local.com" name="Channel 1 Title"/>
		        <item jid="channels.koski.com" node="/user/channel3@local.com" name="Channel 3 Title"/>
		        <item jid="channels.koski.com" node="/user/channel2@local.com" name="Channel 2 Title"/>
	         </query>
	      </iq>
		 */
		
		mockIQ = new IQ();
		mockIQ.setType(IQ.Type.result);
		mockIQ.setID("testLocalDiscoItemsSuccess");
		mockIQ.setFrom("channels.koski.com");
		mockIQ.setTo("tuomas@koski.com/client");
		Element qry = mockIQ.setChildElement("query", JabberDiscoItems.NAMESPACE_URI);
		
		Set<String> possibleItems = new HashSet<String>();
		Element i = qry.addElement("item")
		   .addAttribute("jid", "channels.koski.com")
		   .addAttribute("node", "/user/channel1@local.com")
		   .addAttribute("name", "Channel 1 Title");
		possibleItems.add(i.asXML());
		
		i = qry.addElement("item")
		   .addAttribute("jid", "channels.koski.com")
		   .addAttribute("node", "/user/channel2@local.com")
		   .addAttribute("name", "Channel 2 Title");
		possibleItems.add(i.asXML());
		
		i = qry.addElement("item")
		   .addAttribute("jid", "channels.koski.com")
		   .addAttribute("node", "/user/channel3@local.com")
		   .addAttribute("name", "Channel 3 Title");
		possibleItems.add(i.asXML());
		
		Element q = result.getChildElement();
		
		assertEquals("query", q.getName());
		
		List<Element> elements = q.elements("item");
		
		for (Element element : elements) {
			assertTrue(possibleItems.contains(element.asXML()));
		}
		
	}
	
	public void testLocalDiscoItemsofNode() throws InterruptedException {
		
		String node = "/user/channel1@local.com";
		jedis.sadd(JedisKeys.LOCAL_NODES, node);
		jedis.hset("node:" + node + ":conf", "pubsub#title", "Channel 1 Title");
		
		jedis.sadd("node:" + node + ":itemset", "123456-1");
		jedis.sadd("node:" + node + ":itemset", "123456-2");
		jedis.sadd("node:" + node + ":itemset", "123456-3");
		jedis.sadd("node:" + node + ":itemset", "123456-4");
		jedis.sadd("node:" + node + ":itemset", "123456-5");
		
		JabberDiscoItems discoItemsEngine = new JabberDiscoItems(this.outQueue, this.errorQueue, this.jedis);
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.get);
		mockIQ.setID("testLocalDiscoItemsofNode");
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("tuomas@koski.com/client");
		
		Element query = mockIQ.setChildElement("query", JabberDiscoItems.NAMESPACE_URI);
		query.addAttribute("node", "/user/channel1@local.com");
		
		discoItemsEngine.ingestPacket(mockIQ);
		
		IQ result = (IQ)discoItemsEngine.outQueue.getQueue().poll();
		
		//System.out.println(result.toXML());
		
		/*
		  <iq type="result" 
		      id="testLocalDiscoItemsofNode" 
		      from="channels.koski.com" 
		      to="tuomas@koski.com/client">
		     <query xmlns="http://jabber.org/protocol/disco#items" 
		            node="/user/channel1@local.com">
		        <item jid="channels.koski.com" name="123456-4"/>
		        <item jid="channels.koski.com" name="123456-5"/>
		        <item jid="channels.koski.com" name="123456-2"/>
		        <item jid="channels.koski.com" name="123456-3"/>
		        <item jid="channels.koski.com" name="123456-1"/>
		     </query>
		  </iq>

		 */
		
		mockIQ = new IQ();
		mockIQ.setType(IQ.Type.result);
		mockIQ.setID("testLocalDiscoItemsSuccess");
		mockIQ.setFrom("channels.koski.com");
		mockIQ.setTo("tuomas@koski.com/client");
		Element qry = mockIQ.setChildElement("query", JabberDiscoItems.NAMESPACE_URI);
		
		Set<String> possibleItems = new HashSet<String>();
		Element i = qry.addElement("item")
		   .addAttribute("jid", "channels.koski.com")
		   .addAttribute("name", "123456-1");
		possibleItems.add(i.asXML());
		
		i = qry.addElement("item")
		   .addAttribute("jid", "channels.koski.com")
		   .addAttribute("name", "123456-2");
		possibleItems.add(i.asXML());
		
		i = qry.addElement("item")
		   .addAttribute("jid", "channels.koski.com")
		   .addAttribute("name", "123456-3");
		possibleItems.add(i.asXML());
		
		i = qry.addElement("item")
		   .addAttribute("jid", "channels.koski.com")
		   .addAttribute("name", "123456-4");
		possibleItems.add(i.asXML());
		
		i = qry.addElement("item")
		   .addAttribute("jid", "channels.koski.com")
		   .addAttribute("name", "123456-5");
		possibleItems.add(i.asXML());
		
		Element q = result.getChildElement();
		
		assertEquals("query", q.getName());
		
		List<Element> elements = q.elements("item");
		
		for (Element element : elements) {
			assertTrue(possibleItems.contains(element.asXML()));
		}
		
		//System.out.println(result.toXML());
		
	}
	
}
