package org.buddycloud.channelserver.packetHandler.iq.namespace;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.buddycloud.channelserver.db.jedis.JedisMongoDataStore;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
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
    public void init() throws FileNotFoundException, IOException {
        this.outQueue = new LinkedBlockingQueue<Packet>();
        this.inQueue = new LinkedBlockingQueue<Packet>();
        InQueueConsumer consumer = new InQueueConsumer(outQueue, IQTestHandler.readConf(), inQueue);
        consumer.start();
        
        IQTestHandler.getJedis(); // don't remove, it's here to clean the db
    }
    
    @Test
    public void testDiscoInfo() throws IOException, DocumentException, InterruptedException {
        
        IQ request = IQTestHandler.readStanzaAsIq("/iq/discoInfo/request.stanza");
        String expectedReply = IQTestHandler.readStanzaAsString("/iq/discoInfo/reply.stanza");
        
        inQueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }

    @Test
    public void testDiscoInfoGetNodeMetaData() throws IOException, DocumentException, InterruptedException {
        
        IQ request = IQTestHandler.readStanzaAsIq("/iq/discoInfo/requestNode.stanza");
        String expectedReply = IQTestHandler.readStanzaAsString("/iq/discoInfo/replyNode.stanza");
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQTestHandler.readConf());
        
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
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQTestHandler.readConf());
        dataStore.addLocalUser("francisco@denmark.lit");
        
        IQ request = IQTestHandler.readStanzaAsIq("/iq/discoInfo/foreign/request.stanza");
        
        String itemsRequest = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/itemsRequest.stanza");
        String itemsReply = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/itemsReply.stanza");
        
        String infoRequestOne = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/infoRequest1.stanza");
        String infoReplyOne = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/infoReply1.stanza");
        String infoRequestTwo = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/infoRequest2.stanza");
        String infoReplyTwo = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/infoReply2.stanza");
        
        String subscriptionRequest = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/infoRequestWithNode.stanza");
        String subscriptionReply = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/infoReplyWithNode.stanza");
        
        String expectedReply = IQTestHandler.readStanzaAsString("/iq/discoInfo/foreign/reply.stanza");
        
        // subscription request.
        inQueue.put(request);
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        // fisrt step is to send disco#items
        Assert.assertNotNull(replyIQ);
        itemsRequest = itemsRequest.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(itemsRequest, replyIQ.toXML());
        
        // disco#items reply from foreign server
        itemsReply = itemsReply.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQTestHandler.toIq(itemsReply));
        
        // For each item we send disco#info
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        infoRequestOne = infoRequestOne.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(infoRequestOne, replyIQ.toXML());
        
        // first item was not ok, we send another
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQTestHandler.toIq(infoReplyOne));
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        infoRequestTwo = infoRequestTwo.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(infoRequestTwo, replyIQ.toXML());
        
        // Now we should have subscription request going out
        // becase the second info should find a buddycloud channel component.
        infoReplyTwo = infoReplyTwo.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQTestHandler.toIq(infoReplyTwo));
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        subscriptionRequest = subscriptionRequest.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(subscriptionRequest, replyIQ.toXML());
        
        // Now we have successful subscription reply coming from the foreign channel server,
        // and we will forward it to the user.
        subscriptionReply = subscriptionReply.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQTestHandler.toIq(subscriptionReply));
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
    }
    
}
