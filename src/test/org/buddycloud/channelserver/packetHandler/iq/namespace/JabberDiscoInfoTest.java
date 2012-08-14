package org.buddycloud.channelserver.packetHandler.iq.namespace;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.buddycloud.channelserver.db.jedis.JedisMongoDataStore;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;
import org.buddycloud.channelserver.queue.InQueueConsumer;
import org.dom4j.DocumentException;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class JabberDiscoInfoTest
{
    private LinkedBlockingQueue<Packet> outQueue;
    private LinkedBlockingQueue<Packet> inQueue;

    @Before
    public void init() {
        this.outQueue = new LinkedBlockingQueue<Packet>();
        this.inQueue = new LinkedBlockingQueue<Packet>();
        InQueueConsumer consumer = new InQueueConsumer(outQueue, IQHandlerTest.readConf(), inQueue);
        consumer.start();
        
        IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
    }
    
    @Test
    public void testDiscoInfo() throws IOException, DocumentException, InterruptedException {
        
        IQ request = IQHandlerTest.readStanzaAsIq("/iq/discoInfo/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/discoInfo/reply.stanza");
        
        inQueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }

    @Test
    public void testDiscoInfoGetNodeMetaData() throws IOException, DocumentException, InterruptedException {
        
        IQ request = IQHandlerTest.readStanzaAsIq("/iq/discoInfo/requestNode.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/discoInfo/replyNode.stanza");
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        
        String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        
        dataStore.createUserNodes("romeo@montague.net");
        dataStore.addLocalUser("romeo@montague.net");
        
        inQueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("TIME", sdf.format(new Date()));
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testDiscoInfoGetNodeMetaFromForeignNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("francisco@denmark.lit");
        
        IQ request = IQHandlerTest.readStanzaAsIq("/iq/discoInfo/foreign/request.stanza");
        
        String itemsRequest = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/itemsRequest.stanza");
        String itemsReply = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/itemsReply.stanza");
        
        String infoRequestOne = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoRequest1.stanza");
        String infoReplyOne = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoReply1.stanza");
        String infoRequestTwo = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoRequest2.stanza");
        String infoReplyTwo = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoReply2.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoRequestWithNode.stanza");
        String subscriptionReply = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/infoReplyWithNode.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/discoInfo/foreign/reply.stanza");
        
        // subscription request.
        inQueue.put(request);
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        // fisrt step is to send disco#items
        Assert.assertNotNull(replyIQ);
        itemsRequest = itemsRequest.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(itemsRequest, replyIQ.toXML());
        
        // disco#items reply from foreign server
        itemsReply = itemsReply.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(itemsReply));
        
        // For each item we send disco#info
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        infoRequestOne = infoRequestOne.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(infoRequestOne, replyIQ.toXML());
        
        // first item was not ok, we send another
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(infoReplyOne));
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        infoRequestTwo = infoRequestTwo.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(infoRequestTwo, replyIQ.toXML());
        
        // Now we should have subscription request going out
        // becase the second info should find a buddycloud channel component.
        infoReplyTwo = infoReplyTwo.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(infoReplyTwo));
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        subscriptionRequest = subscriptionRequest.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(subscriptionRequest, replyIQ.toXML());
        
        // Now we have successful subscription reply coming from the foreign channel server,
        // and we will forward it to the user.
        subscriptionReply = subscriptionReply.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(subscriptionReply));
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
    }
    
}
