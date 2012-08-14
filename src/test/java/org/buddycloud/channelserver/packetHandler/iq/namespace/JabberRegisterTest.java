package org.buddycloud.channelserver.packetHandler.iq.namespace;

import java.io.IOException;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.jedis.JedisMongoDataStore;
import org.buddycloud.channelserver.queue.InQueueConsumer;

import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.dom4j.DocumentException;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

import redis.clients.jedis.Jedis;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;

public class JabberRegisterTest {

    private BlockingQueue<Packet> outQueue;
    private BlockingQueue<Packet> inQueue;
    private Jedis jedis;

    @Before
    public void init() {
        this.outQueue = new LinkedBlockingQueue<Packet>();
        this.inQueue = new LinkedBlockingQueue<Packet>();
        InQueueConsumer consumer = new InQueueConsumer(outQueue, IQHandlerTest.readConf(), inQueue);
        consumer.start();
        
        jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
    }
    
    @Test
    public void testRegister() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        
        IQ request = IQHandlerTest.readStanzaAsIq("/iq/register/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/register/reply.stanza");
        
        inQueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
        Assert.assertTrue(jedis.sismember(JedisMongoDataStore.LOCAL_USERS, "channeluser@example.com"));
        
        // Let's test that user has the channels he should
        Assert.assertEquals(Conf.getDefaultPostChannelConf("channeluser@example.com"), 
                     jedis.hgetAll(JedisMongoDataStore.getNodeConfRedisKey(Conf.getPostChannelNodename("channeluser@example.com"))));
        
        NodeSubscriptionImpl ns = dataStore.getUserSubscriptionOfNode("channeluser@example.com", 
                                                                  Conf.getPostChannelNodename("channeluser@example.com"));

        Assert.assertEquals("owner", ns.getAffiliation());
        Assert.assertEquals("unconfigured", ns.getSubscription());
        
    }
    
    public void testRegisterFailsWhenComingFromWrongDomain() throws IOException, DocumentException, InterruptedException {
        
        IQ request = IQHandlerTest.readStanzaAsIq(
                "/iq/register/fail/request-different-domain.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/register/fail/reply-different-domain.stanza");
        
        inQueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testCannotRegisterMoreThanOnce() throws IOException, DocumentException, InterruptedException {
        
        IQ request = IQHandlerTest.readStanzaAsIq("/iq/register/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/register/reply.stanza");
        
        inQueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
        Assert.assertTrue(jedis.sismember(JedisMongoDataStore.LOCAL_USERS, "channeluser@example.com"));
        
        inQueue.put(request);
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        expectedReply = IQHandlerTest.readStanzaAsString("/iq/register/fail/reply-conflict.stanza");
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
    }
    
}
