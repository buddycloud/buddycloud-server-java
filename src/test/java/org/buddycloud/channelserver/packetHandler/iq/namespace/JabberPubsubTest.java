package org.buddycloud.channelserver.packetHandler.iq.namespace;

import java.io.FileNotFoundException;
import java.io.IOException;
import org.buddycloud.channelserver.packetHandler.iq.IQHandlerTest;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.jedis.JedisMongoDataStore;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.queue.InQueueConsumer;
import org.dom4j.DocumentException;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class JabberPubsubTest
{
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
    
    @Test
    public void testSubscribeToLocalNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("pamela@denmark.lit");
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/subscribe/reply.stanza");
        
        String expectedSubscriptionNotification = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/subscriptionNotificationMessage.stanza");
        String expectedAffiliationNotification = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/affiliationNotificationMessage.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
        NodeSubscriptionImpl ns = dataStore.getUserSubscriptionOfNode(
                "francisco@denmark.lit", Conf.getPostChannelNodename("pamela@denmark.lit"));
        
        Assert.assertEquals("member", ns.getAffiliation());
        Assert.assertEquals("unconfigured", ns.getSubscription());
        
        // We should have subscription first
        Message replyMsg = (Message)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        Assert.assertEquals(expectedSubscriptionNotification, replyMsg.toXML());
        
        replyMsg = (Message)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        Assert.assertEquals(expectedAffiliationNotification, replyMsg.toXML());
        
    }
    
    @Test
    public void testSubscribeToForeignNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/request.stanza");
        
        String itemsRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/itemsRequest.stanza");
        String itemsReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/itemsReply.stanza");
        
        String infoRequestOne  = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/infoRequest1.stanza");
        String infoReplyOne  = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/infoReply1.stanza");
        String infoRequestTwo  = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/infoRequest2.stanza");
        String infoReplyTwo  = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/infoReply2.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/subscriptionRequest.stanza");
        String subscriptionReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/subscriptionReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/reply.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
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
    
    @Test
    public void testSubscribeToForeignNodeHighfellow() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/highfellow/request.stanza");
        
        String itemsRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/highfellow/itemsRequest.stanza");
        String itemsReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/highfellow/itemsReply.stanza");
        
        String infoRequestOne = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/highfellow/infoRequest1.stanza");
        String infoReplyOne = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/highfellow/infoReply1.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/highfellow/subscriptionRequest.stanza");
        String subscriptionReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/highfellow/subscriptionReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/highfellow/reply.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
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
        
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(infoReplyOne));
        
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
    
    @Test
    public void testSubscribeToForeignNodeFailsNotOnWhiteList() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/notonwhitelist/request.stanza");
        
        String itemsRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/notonwhitelist/itemsRequest.stanza");
        String itemsReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/notonwhitelist/itemsReply.stanza");
        
        String infoRequestOne = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/notonwhitelist/infoRequest1.stanza");
        String infoReplyOne = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/notonwhitelist/infoReply1.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/notonwhitelist/subscriptionRequest.stanza");
        String subscriptionReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/notonwhitelist/subscriptionReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/notonwhitelist/reply.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
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
        
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(infoReplyOne));
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        subscriptionRequest = subscriptionRequest.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(subscriptionRequest, replyIQ.toXML());
        
        // Now we have successful subscription error reply coming from the foreign channel server,
        // and we will forward it to the user.
        subscriptionReply = subscriptionReply.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(subscriptionReply));
        
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
    }
    
    @Test
    public void testSubscribeToForeignNodeFailOnItems() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/items/request.stanza");
        
        String itemsRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/items/itemsRequest.stanza");
        String itemsReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/items/itemsReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/foreign/fail/items/reply.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        // fisrt step is to send disco#items
        Assert.assertNotNull(replyIQ);
        itemsRequest = itemsRequest.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(itemsRequest, replyIQ.toXML());
        
        // disco#items reply from foreign server
        itemsReply = itemsReply.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(itemsReply));
        
        // Disco items fail, so we just send error back to the
        // end user.
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        expectedReply = expectedReply.replaceAll("items1", replyIQ.getID());
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
    }
    
    @Test
    public void testGetSubscriptoinsOfExistingNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("francisco@denmark.lit");
        dataStore.addLocalUser("francisco@denmark.lit");
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscriptions/requestExistingNode.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscriptions/replyExistingNode.stanza");
        
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testGetSubscriptions() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        dataStore.createUserNodes("pamela@denmark.lit");
        dataStore.subscribeUserToNode(bareJID, Conf.getPostChannelNodename(
                "pamela@denmark.lit"), "member", "unconfigured", null);
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscriptions/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscriptions/reply.stanza");
        
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testPublishToLocalNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("koski@buddycloud.com");
        dataStore.addLocalUser("koski@buddycloud.com");
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/publish/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/publish/reply.stanza");
        String msgNotification = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/publish/msgNotification.stanza");
        
        // IQ result for the publish
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        String itemID = replyIQ.getChildElement().element("publish").element("item").attributeValue("id");
        expectedReply = expectedReply.replaceAll("ITEMID", itemID);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
        // Notifications.
        Message replyMsg = (Message)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        Assert.assertNotNull(replyMsg);
        msgNotification = msgNotification.replaceAll("ITEMID", itemID);
        replyMsg.setID("dontcare");
        String time = replyMsg.getChildElement("event", "http://jabber.org/protocol/pubsub#event")
                              .element("items")
                              .element("item")
                              .element("entry")
                              .element("published").getTextTrim();
        msgNotification = msgNotification.replaceAll("TIME", time);
        Assert.assertEquals(msgNotification, replyMsg.toXML());
        
    }
    
    @Test
    public void testGetItemsEmptyNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/items/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/items/replyNoItems.stanza");
        
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testGetItems() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "1", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,1</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:57Z</published>" +
                             		"<updated>2011-11-27T19:05:57Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc><activity:verb>post</activity:verb>" +
                             		"<activity:object><activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "2", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,2</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:58Z</published>" +
                             		"<updated>2011-11-27T19:05:58Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc><activity:verb>post</activity:verb>" +
                             		"<activity:object><activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "3", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,3</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:58Z</published>" +
                             		"<updated>2011-11-27T19:05:58Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc>" +
                             		"<activity:verb>post</activity:verb><activity:object>" +
                             		"<activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");

        String request = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/reply.stanza");
        
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testGetItemsMax1() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "1", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,1</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:57Z</published>" +
                             		"<updated>2011-11-27T19:05:57Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc>" +
                             		"<activity:verb>post</activity:verb>" +
                             		"<activity:object><activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "2", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,2</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:58Z</published>" +
                             		"<updated>2011-11-27T19:05:58Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc>" +
                             		"<activity:verb>post</activity:verb>" +
                             		"<activity:object><activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "3", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,3</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:58Z</published>" +
                             		"<updated>2011-11-27T19:05:58Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc>" +
                             		"<activity:verb>post</activity:verb>" +
                             		"<activity:object><activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");

        String request = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/requestMax1.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/replyMax1.stanza");
        
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        String id = replyIQ.getChildElement().element("set").element("first").getText();
        expectedReply = expectedReply.replaceAll("ITEMID", id);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
    }
    
    @Test
    public void testGetItemsMax1Rsm() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        String bareJID = "francisco@denmark.lit";
        dataStore.createUserNodes(bareJID);
        dataStore.addLocalUser(bareJID);
        
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "1", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,1</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:57Z</published>" +
                             		"<updated>2011-11-27T19:05:57Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc>" +
                             		"<activity:verb>post</activity:verb><activity:object>" +
                             		"<activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "2", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,2</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:58Z</published>" +
                             		"<updated>2011-11-27T19:05:58Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc>" +
                             		"<activity:verb>post</activity:verb>" +
                             		"<activity:object><activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");
        Thread.sleep(1000);
        dataStore.storeEntry(Conf.getPostChannelNodename(bareJID), 
                             "3", "<entry xmlns=\"http://www.w3.org/2005/Atom\" " +
                             		"xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">" +
                             		"<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,3</id>" +
                             		"<title>Post</title><content>Test</content>" +
                             		"<published>2011-11-27T19:05:58Z</published>" +
                             		"<updated>2011-11-27T19:05:58Z</updated>" +
                             		"<author><name>koski@buddycloud.com</name>" +
                             		"<uri>acct:koski@buddycloud.com</uri>" +
                             		"<activity:object-type>person</activity:object-type></author>" +
                             		"<geoloc xmlns=\"http://jabber.org/protocol/geoloc\">" +
                             		"<text>Paris, France</text><locality>Paris</locality>" +
                             		"<country>France</country></geoloc>" +
                             		"<activity:verb>post</activity:verb>" +
                             		"<activity:object><activity:object-type>note</activity:object-type>" +
                             		"</activity:object>" +
                             		"</entry>");

        String request = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/requestMax1Rsm.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/replyMax1.stanza");
        String requestNext = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/requestNextMax1Rsm.stanza");
        String expectedReplyRsm = IQHandlerTest.readStanzaAsString("/iq/pubsub/items/replyNextMax1Rsm.stanza");
        
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        String id = replyIQ.getChildElement().element("set").element("first").getText();
        expectedReply = expectedReply.replaceAll("ITEMID", id);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
        requestNext = requestNext.replaceAll("ITEMID", id);
        inQueue.put(IQHandlerTest.toIq(requestNext));
        replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        id = replyIQ.getChildElement().element("set").element("first").getText();
        expectedReplyRsm = expectedReplyRsm.replaceAll("ITEMID", id);
        Assert.assertEquals(expectedReplyRsm, replyIQ.toXML());
    }
    
    @Test
    public void testReceiveSubscriptionrequestFromForeignNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/fromforeign/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/subscribe/fromforeign/reply.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());
        
        NodeSubscriptionImpl ns = dataStore.getUserSubscriptionOfNode(
                "tuomas@buddycloud.org", Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"));
        Assert.assertEquals("channels.buddycloud.org", ns.getForeignChannelServer());
        Assert.assertEquals("member", ns.getAffiliation());
        Assert.assertEquals("tuomas@buddycloud.org", ns.getBareJID());
        Assert.assertEquals("unconfigured", ns.getSubscription());
        Assert.assertEquals(Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"), ns.getNode());
    }
    
    @Test
    public void testUnsubscribeToLocalNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("pamela@denmark.lit");
        dataStore.addLocalUser("francisco@denmark.lit");
        dataStore.subscribeUserToNode("francisco@denmark.lit", 
                                      Conf.getPostChannelNodename("pamela@denmark.lit"), 
                                      "member", 
                                      "unconfigured", 
                                      null);
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/reply.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());

        NodeSubscriptionImpl ns = dataStore.getUserSubscriptionOfNode(
                "francisco@denmark.lit", Conf.getPostChannelNodename("pamela@denmark.lit"));
        
        Assert.assertEquals(null, ns.getAffiliation());
        Assert.assertEquals(null, ns.getSubscription());
        
    }
    
    @Test
    public void testUnsubscribeToLocalNodeComesFromForeignChannelServer() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.createUserNodes("tuomas@xmpp.lobstermonster.org");
        dataStore.subscribeUserToNode("tuomas@buddycloud.org", 
                                      Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"), 
                                      "member", 
                                      "unconfigured", 
                                      "channels.buddycloud.org");
        
        NodeSubscriptionImpl ns = dataStore.getUserSubscriptionOfNode(
                "tuomas@buddycloud.org", Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"));

        Assert.assertEquals("member", ns.getAffiliation());
        Assert.assertEquals("unconfigured", ns.getSubscription());
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/fromforeign/request.stanza");
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/fromforeign/reply.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        Assert.assertNotNull(replyIQ);
        Assert.assertEquals(expectedReply, replyIQ.toXML());

        ns = dataStore.getUserSubscriptionOfNode(
                "tuomas@buddycloud.org", Conf.getPostChannelNodename("tuomas@xmpp.lobstermonster.org"));
        
        Assert.assertEquals(null, ns.getAffiliation());
        Assert.assertEquals(null, ns.getSubscription());
        
    }
    
    @Test
    public void testUnsubscribeFromForeignNode() throws IOException, DocumentException, InterruptedException {
        
        JedisMongoDataStore dataStore = new JedisMongoDataStore(IQHandlerTest.readConf());
        dataStore.addLocalUser("tuomas@xmpp.lobstermonster.org");
        dataStore.subscribeUserToNode("tuomas@xmpp.lobstermonster.org", 
                Conf.getPostChannelNodename("tuomas@buddycloud.org"), 
                "member", 
                "unconfigured", 
                null);
        
        NodeSubscriptionImpl ns = dataStore.getUserSubscriptionOfNode(
                "tuomas@xmpp.lobstermonster.org", Conf.getPostChannelNodename("tuomas@buddycloud.org"));

        Assert.assertEquals("member", ns.getAffiliation());
        Assert.assertEquals("unconfigured", ns.getSubscription());
        
        String request = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/foreign/request.stanza");
        
        String itemsRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/foreign/itemsRequest.stanza");
        String itemsReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/foreign/itemsReply.stanza");
        
        String infoRequestOne = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/foreign/infoRequest1.stanza");
        String infoReplyOne   = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/foreign/infoReply1.stanza");
        
        String subscriptionRequest = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/foreign/unsubscriptionRequest.stanza");
        String subscriptionReply   = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/foreign/unsubscriptionReply.stanza");
        
        String expectedReply = IQHandlerTest.readStanzaAsString(
                "/iq/pubsub/unsubscribe/foreign/reply.stanza");
        
        // subscription request.
        inQueue.put(IQHandlerTest.toIq(request));
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
        
        infoReplyOne = infoReplyOne.replaceAll("items1", replyIQ.getID());
        inQueue.put(IQHandlerTest.toIq(infoReplyOne));
        
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
        
        ns = dataStore.getUserSubscriptionOfNode("tuomas@xmpp.lobstermonster.org", 
                Conf.getPostChannelNodename("tuomas@buddycloud.org"));

        Assert.assertEquals(null, ns.getAffiliation());
        Assert.assertEquals(null, ns.getSubscription());
    }
}