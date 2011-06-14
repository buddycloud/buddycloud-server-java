package test.org.buddycloud.channels.packetHandler.IQ;

import junit.framework.TestCase;

import org.buddycloud.channels.packetHandler.IQ.IQHandler;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsub;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberRegister;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.dom4j.Element;
import org.junit.After;
import org.junit.Before;
import org.xmpp.packet.IQ;

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
	
	
	
}
