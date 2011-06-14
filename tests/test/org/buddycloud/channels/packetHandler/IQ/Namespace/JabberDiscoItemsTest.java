package test.org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoInfo;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoItems;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
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
		store.put("type", "subscribe-items");
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
		assertEquals("subscribe-info", store.get("type"));
		assertEquals("bc.heriveau.fr", store.get("components"));
		
		IQ expectedIQ = new IQ();
		expectedIQ.setID(discoInfoReq.getID());
		expectedIQ.setType(Type.get);
		expectedIQ.setTo("people.heriveau.fr");
		expectedIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
		
		assertEquals(expectedIQ.toXML(), discoInfoReq.toXML());
		
	}

}
