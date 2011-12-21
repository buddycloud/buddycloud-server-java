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
import org.xmpp.packet.Message;

import redis.clients.jedis.Jedis;

public class JabberPubsubTest extends TestCase {

    public void testSubscribeToLocalNode() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("pamela@denmark.lit");
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/request.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/reply.stanza");
        
        String expectedSubscriptionNotification = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/subscriptionNotificationMessage.stanza");
        String expectedAffiliationNotification = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/affiliationNotificationMessage.stanza");
        
        // subscription request.
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
        

        NodeSubscription ns = dataStore.getUserSubscriptionOfNode("francisco@denmark.lit", 
                                                                  Conf.getPostChannelNodename("pamela@denmark.lit"));
        
        assertEquals("member", ns.getAffiliation());
        assertEquals("unconfigured", ns.getSubscription());
        
        // We should have subscription first
        Message replyMsg = (Message)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        assertEquals(expectedSubscriptionNotification, replyMsg.toXML());
        
        replyMsg = (Message)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        assertEquals(expectedAffiliationNotification, replyMsg.toXML());
        
    }
    
    public void testSubscribeToForeignNode() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis();
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/request.stanza");
        
        String itemsRequest  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/itemsRequest.stanza");
        String itemsReply    = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/itemsReply.stanza");
        
        String infoRequestOne  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/infoRequest1.stanza");
        String infoReplyOne  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/infoReply1.stanza");
        String infoRequestTwo  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/infoRequest2.stanza");
        String infoReplyTwo  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/infoReply2.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/subscriptionRequest.stanza");
        String subscriptionReply   = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/subscriptionReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/reply.stanza");
        
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
    
    public void testSubscribeToForeignNodeHighfellow() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis();
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/highfellow/request.stanza");
        
        String itemsRequest  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/highfellow/itemsRequest.stanza");
        String itemsReply    = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/highfellow/itemsReply.stanza");
        
        String infoRequestOne  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/highfellow/infoRequest1.stanza");
        String infoReplyOne  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/highfellow/infoReply1.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/highfellow/subscriptionRequest.stanza");
        String subscriptionReply   = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/highfellow/subscriptionReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/highfellow/reply.stanza");
        
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
        
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inqueue.put(infoReplyOne);
        
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
    
    public void testSubscribeToForeignNodeFailsNotOnWhiteList() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis();
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/notonwhitelist/request.stanza");
        
        String itemsRequest  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/notonwhitelist/itemsRequest.stanza");
        String itemsReply    = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/notonwhitelist/itemsReply.stanza");
        
        String infoRequestOne  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/notonwhitelist/infoRequest1.stanza");
        String infoReplyOne  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/notonwhitelist/infoReply1.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/notonwhitelist/subscriptionRequest.stanza");
        String subscriptionReply   = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/notonwhitelist/subscriptionReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/notonwhitelist/reply.stanza");
        
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
        
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inqueue.put(infoReplyOne);
        
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        subscriptionRequest = subscriptionRequest.replaceAll("items1", replyIQ.getID());
        assertEquals(subscriptionRequest, replyIQ.toXML());
        
        // Now we have successful subscription error reply coming from the foreign channel server,
        // and we will forward it to the user.
        subscriptionReply = subscriptionReply.replaceAll("items1", replyIQ.getID());
        inqueue.put(subscriptionReply);
        
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        assertEquals(expectedReply, replyIQ.toXML());
        
    }
    
    public void testSubscribeToForeignNodeFailOnItems() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis();
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/items/request.stanza");
        
        String itemsRequest  = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/items/itemsRequest.stanza");
        String itemsReply    = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/items/itemsReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/foreign/fail/items/reply.stanza");
        
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
        
        // Disco items fail, so we just send error back to the
        // end user.
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        assertEquals(expectedReply, replyIQ.toXML());
        
    }
    
    public void testGetSubscriptoinsOfExistingNode() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("francisco@denmark.lit");
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscriptions/requestExistingNode.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscriptions/replyExistingNode.stanza");
        
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testGetSubscriptions() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        dataStore.createUserNodes("pamela@denmark.lit");
        dataStore.subscribeUserToNode(bareJID, Conf.getPostChannelNodename("pamela@denmark.lit"), "member", "unconfigured", null);
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscriptions/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscriptions/reply.stanza");
        
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testPublishToLocalNode() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("koski@buddycloud.com");
        dataStore.addLocalUser("koski@buddycloud.com");
        
        String request         = IQHandlerTest.readStanzaAsString("/iq/pubsub/publish/request.stanza");
        String expectedReply   = IQHandlerTest.readStanzaAsString("/iq/pubsub/publish/reply.stanza");
        String msgNotification = IQHandlerTest.readStanzaAsString("/iq/pubsub/publish/msgNotification.stanza");
        
        // IQ result for the publish
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        String itemID = replyIQ.getChildElement().element("publish").element("item").attributeValue("id");
        expectedReply = expectedReply.replaceAll("ITEMID", itemID);
        assertEquals(expectedReply, replyIQ.toXML());
        
        // Notifications.
        Message replyMsg = (Message)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        assertNotNull(replyMsg);
        msgNotification = msgNotification.replaceAll("ITEMID", itemID);
        replyMsg.setID("dontcare");
        String time = replyMsg.getChildElement("event", "http://jabber.org/protocol/pubsub#event")
                              .element("items")
                              .element("item")
                              .element("entry")
                              .element("published").getTextTrim();
        msgNotification = msgNotification.replaceAll("TIME", time);
        assertEquals(msgNotification, replyMsg.toXML());
        
    }
    
    public void testGetItemsEmptyNode() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/replyNoItems.stanza");
        
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testGetItems() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "1", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,1</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:57Z</published><updated>2011-11-27T19:05:57Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "2", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,2</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:58Z</published><updated>2011-11-27T19:05:58Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "3", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,3</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:58Z</published><updated>2011-11-27T19:05:58Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");

        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/reply.stanza");
        
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testGetItemsMax1() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "1", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,1</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:57Z</published><updated>2011-11-27T19:05:57Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "2", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,2</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:58Z</published><updated>2011-11-27T19:05:58Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "3", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,3</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:58Z</published><updated>2011-11-27T19:05:58Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");

        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/requestMax1.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/replyMax1.stanza");
        
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        String id = replyIQ.getChildElement().element("set").element("first").getText();
        expectedReply = expectedReply.replaceAll("ITEMID", id);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
    public void testGetItemsMax1Rsm() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "1", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,1</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:57Z</published><updated>2011-11-27T19:05:57Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "2", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,2</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:58Z</published><updated>2011-11-27T19:05:58Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "3", "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\"><id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,3</id><title>Post</title><content>Test</content><published>2011-11-27T19:05:58Z</published><updated>2011-11-27T19:05:58Z</updated><author><name>koski@buddycloud.com</name><uri>acct:koski@buddycloud.com</uri><activity:object-type>person</activity:object-type></author><geoloc xmlns=\"http://jabber.org/protocol/geoloc\"><text>Paris, France</text><locality>Paris</locality><country>France</country></geoloc><activity:verb>post</activity:verb><activity:object><activity:object-type>note</activity:object-type></activity:object></entry>");

        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/requestMax1Rsm.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/replyMax1.stanza");
        String requestNext   = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/requestNextMax1Rsm.stanza");
        String expectedReplyRsm = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/replyNextMax1Rsm.stanza");
        
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        String id = replyIQ.getChildElement().element("set").element("first").getText();
        expectedReply = expectedReply.replaceAll("ITEMID", id);
        assertEquals(expectedReply, replyIQ.toXML());
        
        requestNext = requestNext.replaceAll("ITEMID", id);
        inqueue.put(requestNext);
        replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        id = replyIQ.getChildElement().element("set").element("first").getText();
        expectedReplyRsm = expectedReplyRsm.replaceAll("ITEMID", id);
        assertEquals(expectedReplyRsm, replyIQ.toXML());
    }
    
    public void testReceiveSubscriptionrequestFromForeignNode() throws IOException, DocumentException, InterruptedException {
        Jedis jedis = IQHandlerTest.getJedis();
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/fromforeign/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/fromforeign/reply.stanza");
        
        // subscription request.
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
        
        NodeSubscription ns = dataStore.getUserSubscriptionOfNode("tuomas@buddycloud.org", Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"));
        assertEquals("channels.buddycloud.org", ns.getForeignChannelServer());
        assertEquals("member", ns.getAffiliation());
        assertEquals("tuomas@buddycloud.org", ns.getBareJID());
        assertEquals("unconfigured", ns.getSubscription());
        assertEquals(Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"), ns.getNode());
    }
    
    public void testUnsubscribeToLocalNode() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("pamela@denmark.lit");
        dataStore.addLocalUser("francisco@denmark.lit");
        dataStore.subscribeUserToNode("francisco@denmark.lit", 
                                      Conf.getPostChannelNodename("pamela@denmark.lit"), 
                                      "member", 
                                      "unconfigured", 
                                      null);
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/request.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/reply.stanza");
        
        // subscription request.
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());

        NodeSubscription ns = dataStore.getUserSubscriptionOfNode("francisco@denmark.lit", 
                                                                  Conf.getPostChannelNodename("pamela@denmark.lit"));
        
        assertEquals(null, ns.getAffiliation());
        assertEquals(null, ns.getSubscription());
        
    }
    
    public void testUnsubscribeToLocalNodeComesFromForeignChannelServer() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis(); // don't remove, it's here to clean the db
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        dataStore.subscribeUserToNode("tuomas@buddycloud.org", 
                                      Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"), 
                                      "member", 
                                      "unconfigured", 
                                      "channels.buddycloud.org");
        
        NodeSubscription ns = dataStore.getUserSubscriptionOfNode("tuomas@buddycloud.org", 
                                                                  Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"));

        assertEquals("member", ns.getAffiliation());
        assertEquals("unconfigured", ns.getSubscription());
        
        String request       = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/fromforeign/request.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/fromforeign/reply.stanza");
        
        // subscription request.
        inqueue.put(request);
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());

        ns = dataStore.getUserSubscriptionOfNode("tuomas@buddycloud.org", 
                                                 Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"));
        
        assertEquals(null, ns.getAffiliation());
        assertEquals(null, ns.getSubscription());
        
    }
    
    
    public void testUnsubscribeFromForeignNode() throws IOException, DocumentException, InterruptedException {
        
        Jedis jedis = IQHandlerTest.getJedis();
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, IQHandlerTest.readConf());
        
        DataStore dataStore = new DataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.subscribeUserToNode("tuomas@xmpp.lobstermonster.org", 
                Conf.getPostChannelNodename("tuomas@buddycloud.org"), 
                "member", 
                "unconfigured", 
                null);
        
        NodeSubscription ns = dataStore.getUserSubscriptionOfNode("tuomas@xmpp.lobstermonster.org", 
                                                                  Conf.getPostChannelNodename("tuomas@buddycloud.org"));

        assertEquals("member", ns.getAffiliation());
        assertEquals("unconfigured", ns.getSubscription());
        
        String request      = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/foreign/request.stanza");
        
        String itemsRequest = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/foreign/itemsRequest.stanza");
        String itemsReply   = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/foreign/itemsReply.stanza");
        
        String infoRequestOne = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/foreign/infoRequest1.stanza");
        String infoReplyOne   = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/foreign/infoReply1.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/foreign/unsubscriptionRequest.stanza");
        String subscriptionReply   = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/foreign/unsubscriptionReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/unsubscribe/foreign/reply.stanza");
        
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
        
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inqueue.put(infoReplyOne);
        
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
        
        ns = dataStore.getUserSubscriptionOfNode("tuomas@xmpp.lobstermonster.org", 
                Conf.getPostChannelNodename("tuomas@buddycloud.org"));

        assertEquals(null, ns.getAffiliation());
        assertEquals(null, ns.getSubscription());
    }
    
}
