package org.buddycloud.channelserver.packetHandler.IQ.Namespace;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetHandler.IQ.IQHandlerTest;
import org.buddycloud.channelserver.queue.InQueue;
import org.buddycloud.channelserver.queue.TestOutQueue;
import org.dom4j.DocumentException;
import org.xmpp.packet.IQ;

import redis.clients.jedis.Jedis;

public class JabberDiscoInfoTest extends TestCase {

    public void testDiscoInfo() throws IOException, DocumentException, InterruptedException {
        
        String request   = IQHandlerTest.readStanzaAsString("/iq/discoInfo/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/discoInfo/reply.stanza");
        
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        inqueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testDiscoInfoGetNodeMetaData() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        String request   = IQHandlerTest.readStanzaAsString("/iq/discoInfo/requestNode.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/discoInfo/replyNode.stanza");
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        dataStore.createUserNodes("romeo@montague.net");
        dataStore.addLocalUser("romeo@montague.net");
        
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        inqueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("TIME", sdf.format(new Date()));
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testDiscoInfoGetNodeMetaFromForeignNode() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis();
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/request.stanza");
        
        String itemsRequest  = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/itemsRequest.stanza");
        String itemsReply    = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/itemsReply.stanza");
        
        String infoRequestOne  = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoRequest1.stanza");
        String infoReplyOne  = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoReply1.stanza");
        String infoRequestTwo  = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoRequest2.stanza");
        String infoReplyTwo  = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoReply2.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoRequestWithNode.stanza");
        String subscriptionReply   = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoReplyWithNode.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/reply.stanza");
        
        // subscription request.
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        // fisrt step is to send disco#items
        assertNotNull(replyIQ);
        itemsRequest = itemsRequest.replaceAll("items1", replyIQ.getID());
        assertEquals(itemsRequest, replyIQ.toXML());
        
        // disco#items reply from foreign server
        itemsReply = itemsReply.replaceAll("items1", replyIQ.getID());
        inqueue.put(itemsReply);
        
        // For each item we send disco#info
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        infoRequestOne = infoRequestOne.replaceAll("items1", replyIQ.getID());
        assertEquals(infoRequestOne, replyIQ.toXML());
        
        // first item was not ok, we send another
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inqueue.put(infoReplyOne);
        
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        infoRequestTwo = infoRequestTwo.replaceAll("items1", replyIQ.getID());
        assertEquals(infoRequestTwo, replyIQ.toXML());
        
        // Now we should have subscription request going out
        // becase the second info should find a buddycloud channel component.
        infoReplyTwo = infoReplyTwo.replaceAll("items1", replyIQ.getID());
        inqueue.put(infoReplyTwo);
        
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        subscriptionRequest = subscriptionRequest.replaceAll("items1", replyIQ.getID());
        assertEquals(subscriptionRequest, replyIQ.toXML());
        
        // Now we have successful subscription reply coming from the foreign channel server,
        // and we will forward it to the user.
        subscriptionReply = subscriptionReply.replaceAll("items1", replyIQ.getID());
        inqueue.put(subscriptionReply);
        
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        assertEquals(expectedReply, replyIQ.toXML());
        
        
    }
    
}
