package test.org.buddycloud.channels.packetHandler.Message;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.buddycloud.channels.packetHandler.Message.MessageHandler;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.junit.After;
import org.junit.Before;
import org.xmpp.packet.Message;

import redis.clients.jedis.Jedis;


public class MessageHandlerTest extends TestCase {

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

	public void testRemotePubsubEventNewMessage() {
		
		String id = "testRemotePubsubEventNewMessage";
		MessageHandler messageHandler = new MessageHandler(outQueue, errorQueue, jedis);
		
		Message msg = new Message();
		msg.setID(id);
		msg.setTo("channels.koski.com");
		msg.setFrom("bc.heriveau.fr");
		msg.setType(Message.Type.headline);
		
		Element event = msg.addChildElement("event", "http://jabber.org/protocol/pubsub#event");
		Element items = event.addElement("items");
		items.addAttribute("node", "/user/nelly@heriveau.fr/status");
		Element item = items.addElement("item");
		item.addAttribute("id", "0123-4567-89");
		
		Element entry = new DOMElement("entry", new org.dom4j.Namespace("", "http://www.w3.org/2005/Atom"));
		//entry.add(new org.dom4j.Namespace("activity", "http://activitystrea.ms/spec/1.0/"));
		
		String DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
		SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		
		entry.addElement("id").setText("1");
		entry.addElement("title").setText("Status update.");
		entry.addElement("content").setText("This is my new Status!");
		entry.addElement("updated").setText(sdf.format(new Date()));
		
//		entry.addElement("author")
//             .addElement("name")
//		     .setText("Nelly");
		
		//System.out.println(entry.asXML());
	}

}
