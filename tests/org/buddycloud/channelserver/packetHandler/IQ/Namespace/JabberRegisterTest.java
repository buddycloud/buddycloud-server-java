package org.buddycloud.channelserver.packetHandler.IQ.Namespace;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetHandler.IQ.IQHandlerTest;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.queue.InQueue;
import org.buddycloud.channelserver.queue.TestOutQueue;
import org.dom4j.DocumentException;
import org.xmpp.packet.IQ;

import redis.clients.jedis.Jedis;

public class JabberRegisterTest extends TestCase {

    public void testRegister() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis();
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/register/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/register/reply.stanza");
        
        inqueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
        
        assertTrue(jedis.sismember(DataStore.LOCAL_USERS, "channeluser@example.com"));
        
        // Let's test that user has the channels he should
        assertEquals(Conf.getDefaultPostChannelConf("channeluser@example.com"), 
                     jedis.hgetAll(DataStore.getNodeConfRedisKey(Conf.getPostChannelNodename("channeluser@example.com"))));
        
        NodeSubscription ns = dataStore.getUserSubscriptionOfNode("channeluser@example.com", 
                                                                  Conf.getPostChannelNodename("channeluser@example.com"));

        assertEquals("owner", ns.getAffiliation());
        assertEquals("unconfigured", ns.getSubscription());
        
    }
    
    public void testRegisterFailsWhenComingFromWrongDomain() throws IOException, DocumentException, InterruptedException {
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/register/fail/request-different-domain.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/register/fail/reply-different-domain.stanza");
        
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        inqueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testCannotRegisterMoreThanOnce() throws IOException, DocumentException, InterruptedException {
        

        Jedis jedis = IQHandlerTest.getJedis();
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/register/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/register/reply.stanza");
        
        inqueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
        
        assertTrue(jedis.sismember(DataStore.LOCAL_USERS, "channeluser@example.com"));
        
        inqueue.put(request);
        
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        expectedReply = IQHandlerTest.readStanzaAsString("/iq/register/fail/reply-conflict.stanza");
        assertEquals(expectedReply, replyIQ.toXML());
        
    }
    
}
