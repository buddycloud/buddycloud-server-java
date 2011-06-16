package test.org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoInfo;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsub;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.State;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.junit.After;
import org.junit.Before;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;

import redis.clients.jedis.Jedis;


public class JabberDiscoInfoTest extends TestCase {

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

	public void testGetDiscoInfoSuccess() {
		
		String id = "testGetDiscoInfoSuccess";
		
		JabberDiscoInfo discoInfoEngine = new JabberDiscoInfo(this.outQueue, this.errorQueue, this.jedis);
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.get);
		mockIQ.setID(id);
		mockIQ.setFrom("channels.buddycloud.com");
		mockIQ.setTo("channels.koski.com");
		
		mockIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
		
		discoInfoEngine.ingestPacket(mockIQ.createCopy());
		
		IQ result = (IQ)discoInfoEngine.outQueue.getQueue().poll();
		
		assertEquals(IQ.Type.result, result.getType());
		assertEquals(id, result.getID());
		
		String expected = "<iq type=\"result\" id=\"testGetDiscoInfoSuccess\" from=\"channels.koski.com\" to=\"channels.buddycloud.com\"><query xmlns=\"http://jabber.org/protocol/disco#info\"><identity category=\"pubsub\" type=\"channels\" name=\"Koski's buddycloud channel server!\"/><feature var=\"http://jabber.org/protocol/disco#info\"/></query></iq>";
		assertEquals(expected, result.toXML());
		
		//System.out.println(result.toXML());
	}
	
	public void testResultlDiscoInfoNotBuddycloudChannleAndRequestNextInfoRequest() throws InterruptedException {
		
		String id = "testRestulDiscoInfoSuccess";
		
		Map <String, String> store = new HashMap<String, String>();
		store.put(State.KEY_STATE, State.STATE_DISCO_INFO_TO_COMPONENTS);
		store.put("id", "original-subs-id-123456");
		store.put("jid", "tuomas@koski.com/client");
		store.put("node", "/user/nelly@heriveau.fr/status");
		store.put(State.KEY_COMPONENTS, "bc.heriveau.fr");
		
		jedis.hmset("store:" + id, store);
		
		JabberDiscoInfo discoInfoEngine = new JabberDiscoInfo(this.outQueue, this.errorQueue, this.jedis);
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.result);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("people.heriveau.fr");
		
		Element query = mockIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
		query.addElement("identity")
			 .addAttribute("category", "humppaa")
			 .addAttribute("type", "numbbaa");
		
		discoInfoEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ discoInfoReq = (IQ)discoInfoEngine.outQueue.getQueue().poll();
		//System.out.println(discoInfoReq.toXML());
		
		assertEquals(IQ.Type.get, discoInfoReq.getType());
		
		store = jedis.hgetAll("store:" + discoInfoReq.getID());
		
		assertEquals("original-subs-id-123456", store.get("id"));
		assertEquals("/user/nelly@heriveau.fr/status", store.get("node"));
		assertEquals("tuomas@koski.com/client", store.get("jid"));
		assertEquals(State.STATE_DISCO_INFO_TO_COMPONENTS, store.get(State.KEY_STATE));
		assertEquals("", store.get("components"));
		
		IQ expectedIQ = new IQ();
		expectedIQ.setID(discoInfoReq.getID());
		expectedIQ.setType(Type.get);
		expectedIQ.setTo("bc.heriveau.fr");
		expectedIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
		
		assertEquals(expectedIQ.toXML(), discoInfoReq.toXML());
	}
	
	public void testResultDiscoInfoFoundsBuddycloudChannelAndSendsSubscribe() throws InterruptedException {
		
		String id = "testResultDiscoInfoFoundsBuddycloudChannelAndSendsSubscribe";
		
		Map <String, String> store = new HashMap<String, String>();
		store.put(State.KEY_STATE, State.STATE_DISCO_INFO_TO_COMPONENTS);
		store.put("id", "original-subs-id-123456");
		store.put("jid", "tuomas@koski.com/client");
		store.put("node", "/user/nelly@heriveau.fr/status");
		store.put(State.KEY_COMPONENTS, "");
		
		jedis.hmset("store:" + id, store);
		
		JabberDiscoInfo discoInfoEngine = new JabberDiscoInfo(this.outQueue, this.errorQueue, this.jedis);
		
		IQ mockIQ = new IQ();
		mockIQ.setType(IQ.Type.result);
		mockIQ.setID(id);
		mockIQ.setTo("channels.koski.com");
		mockIQ.setFrom("bc.heriveau.fr");
		
		Element query = mockIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
		query.addElement("identity")
			 .addAttribute("category", "pubsub")
			 .addAttribute("type", "channels");
		
		//System.out.println(mockIQ.toXML());
		
		discoInfoEngine.ingestPacket(mockIQ.createCopy());
		
		Thread.sleep(50);
		
		IQ subsReq = (IQ)discoInfoEngine.outQueue.getQueue().poll();
		
		//System.out.println(subsReq.toXML());
		
		Element pubsub = new DOMElement("pubsub", new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));
		pubsub.add(new org.dom4j.Namespace("bc", "http://buddycloud.org/v1"));
		pubsub.addElement("subscribe")
		      .addAttribute("node", "/user/nelly@heriveau.fr/status")
		      .addAttribute("jid", "channels.koski.com")
		      .addAttribute("bc:actor", "tuomas@koski.com");
		
		assertEquals(null, subsReq.getFrom());
		assertEquals("bc.heriveau.fr", subsReq.getTo().toBareJID());
		assertEquals(pubsub.asXML(), subsReq.getChildElement().asXML());
		
		//System.out.println(subsReq.toXML());
		
		store = jedis.hgetAll("store:" + subsReq.getID());
		assertEquals(store.get(State.KEY_STATE), "subscribe");
		assertEquals(store.get("id"), "original-subs-id-123456");
		assertEquals(store.get("jid"), "tuomas@koski.com/client");
		assertEquals(store.get("node"), "/user/nelly@heriveau.fr/status");
		
	}
}
