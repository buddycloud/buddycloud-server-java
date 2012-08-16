package org.buddycloud.channelserver.packetHandler.message;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.apache.commons.io.IOUtils;
import org.buddycloud.channelserver.db.jedis.JedisMongoDataStore;
import org.buddycloud.channelserver.queue.InQueueConsumer;
import org.custommonkey.xmlunit.Diff;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;

public class MessageHandlerTest {

    private LinkedBlockingQueue<Packet> outQueue;
    private LinkedBlockingQueue<Packet> inQueue;

    @Before
    public void init() throws FileNotFoundException, IOException {
        this.outQueue = new LinkedBlockingQueue<Packet>();
        this.inQueue = new LinkedBlockingQueue<Packet>();
        InQueueConsumer consumer = new InQueueConsumer(outQueue, IQHandlerTest.readConf(), inQueue);
        consumer.start();
        
        IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
    }
    
    public static Message readStanzaAsMessage(String stanzaPath) throws IOException, DocumentException {
        
        String stanzaStr = IOUtils.toString(
                new FileInputStream(IQHandlerTest.STANZA_PATH + stanzaPath));
        
        SAXReader xmlReader = new SAXReader();
        xmlReader.setMergeAdjacentText(true);
        xmlReader.setStringInternEnabled(true);
        xmlReader.setStripWhitespaceText(true);
        Element entry = null;
        
        entry = xmlReader.read(new StringReader(stanzaStr)).getRootElement();
        
        return new Message(entry);
        
    }
    
    @Test
    public void testPublishToLocalNodeSuccess() throws Exception {
        
        Message request = readStanzaAsMessage("/message/publish/local/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/message/publish/local/reply.stanza");
  
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        
        inQueue.put(request);
        IQ replyIQ = (IQ)outQueue.poll(2000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        String itemID = replyIQ.getChildElement().element("publish").element("item").attributeValue("id");
        expectedReply = expectedReply.replaceAll("ITEMID", itemID);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testPublishToForeignNodeNodeStartsDiscovery() throws IOException, DocumentException, InterruptedException {
        
        Message request = readStanzaAsMessage("/message/publish/foreign/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/message/publish/foreign/itemsRequest.stanza");
  
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        
        inQueue.put(request);
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testOnPubsubEventPublish() throws Exception {
        
        Message request = readStanzaAsMessage("/message/publish/event/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/message/publish/event/reply.stanza");
  
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        dataStore.subscribeUserToNode("tuomas@xmpp.lobstermonster.org", 
                                      "/user/test@highfellow.org/posts", 
                                      null,  
                                      "unconfigured", 
                                      "buddycloud.highfellow.org");
        
        inQueue.put(request);
        Message replyMsg = (Message)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyMsg);
        //expectedReply = expectedReply.replaceAll("items1", replyMsg.getID());
        Diff diff = new Diff(expectedReply, replyMsg.toXML());
        Assert.assertTrue(diff.toString(), diff.identical());
    }
    
}
