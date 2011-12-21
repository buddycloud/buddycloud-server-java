package org.buddycloud.channelserver.packetHandler.Message;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetHandler.IQ.IQHandlerTest;
import org.buddycloud.channelserver.queue.InQueue;
import org.buddycloud.channelserver.queue.TestOutQueue;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;

import redis.clients.jedis.Jedis;

public class MessageHandlerTest extends TestCase {

    public static Message readStanzaAsMessage(String stanza) throws IOException, DocumentException {
        
        FileInputStream fstream = new FileInputStream(IQHandlerTest.STANZA_PATH + stanza);
        
        DataInputStream in = new DataInputStream(fstream);
        BufferedReader br = new BufferedReader(new InputStreamReader(in));
        
        StringBuilder sb = new StringBuilder();
        
        String strLine = "";
        while ((strLine = br.readLine()) != null) {
            sb.append(strLine);
        }
        
        if (in != null) {
            in.close();
        }

        in = null;
        
        SAXReader xmlReader = new SAXReader();
        xmlReader.setMergeAdjacentText(true);
        xmlReader.setStringInternEnabled(true);
        xmlReader.setStripWhitespaceText(true);
        Element entry = null;
        
        entry = xmlReader.read(new StringReader(sb.toString())).getRootElement();
        
        return new Message(entry);
        
    }
    
//    public void testSubscribeToForeignNodeSuccess() throws IOException, InterruptedException, DocumentException {
//        
//        String request       = IQHandlerTest.readStanzaAsString("/message/subscribe/request.stanza");
//        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/featureNotImplemented/reply.stanza");
//        
//        TestOutQueue outQueue = new TestOutQueue();
//        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
//        
//        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
//        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
//        
//        inqueue.put(request);
//        
//        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
//        
//        assertNotNull(replyIQ);
//        assertEquals(expectedReply, replyIQ.toXML());
//    }
    
    public void testPublishToLocalNodeSuccess() throws IOException, DocumentException, InterruptedException {
        
        String request       = IQHandlerTest.readStanzaAsString("/message/publish/local/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/message/publish/local/reply.stanza");
  
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
  
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        
        
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        String itemID = replyIQ.getChildElement().element("publish").element("item").attributeValue("id");
        expectedReply = expectedReply.replaceAll("ITEMID", itemID);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testPublishToForeignNodeNodeStartsDiscovery() throws IOException, DocumentException, InterruptedException {
        
        String request       = IQHandlerTest.readStanzaAsString("/message/publish/foreign/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/message/publish/foreign/itemsRequest.stanza");
  
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
  
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testOnPubsubEventPublish() throws IOException, DocumentException, InterruptedException {
        
        String request       = IQHandlerTest.readStanzaAsString("/message/publish/event/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/message/publish/event/reply.stanza");
  
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
  
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        dataStore.subscribeUserToNode("tuomas@xmpp.lobstermonster.org", 
                                      "/user/test@highfellow.org/posts", 
                                      null,  
                                      "unconfigured", 
                                      "buddycloud.highfellow.org");
        
        inqueue.put(request);
        Message replyMsg = (Message)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyMsg);
        //expectedReply = expectedReply.replaceAll("items1", replyMsg.getID());
        assertEquals(expectedReply, replyMsg.toXML());
    }
    
}
