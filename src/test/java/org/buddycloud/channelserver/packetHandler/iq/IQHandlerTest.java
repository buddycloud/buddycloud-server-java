package org.buddycloud.channelserver.packetHandler.iq;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.net.UnknownHostException;
import java.util.Properties;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.buddycloud.channelserver.queue.InQueueConsumer;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

import redis.clients.jedis.Jedis;

import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.Mongo;
import com.mongodb.MongoException;

public class IQHandlerTest extends TestCase {

    public final static String STANZA_PATH = "src/test/java/resources/stanzas";
    
    public static void dropMongodb() {
        Properties conf = readConf();
        
        Mongo mongo = null;
        try {
            mongo = new Mongo(conf.getProperty("mongo.host"), 
                    Integer.parseInt(conf.getProperty("mongo.port")));
        } catch (NumberFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (MongoException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        DB mdb = mongo.getDB(conf.getProperty("mongo.db"));
        //mdb.dropDatabase();
        DBCollection subscriptions = mdb.getCollection("subscriptions");
        subscriptions.drop();
        DBCollection entries = mdb.getCollection("entries");
        entries.drop();
    }
    
    /**
     * Side-effect: resets Jedis
     * @return
     */
    public static Jedis getJedis() {
        Properties conf = readConf();
        Jedis jedis = new Jedis(conf.getProperty("redis.host"), 
                Integer.valueOf(conf.getProperty("redis.port")));
        jedis.configSet("timeout", "0");
        
        jedis.flushAll();
        dropMongodb();
        
        return jedis;
    }
    
    public static Properties readConf() {
        Properties conf = new Properties();
        try {
            conf.load(new FileInputStream("src/test/java/resources/configuration.properties"));
        } catch (IOException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
        return conf;
    }
    
    public static String readStanzaAsString(String stanzaPath) throws IOException, DocumentException {
        String stanzaStr = IOUtils.toString(
                new FileInputStream(STANZA_PATH + stanzaPath));
        return stanzaStr.replaceAll("   ", "").replaceAll("\n", "");
    }
    
    public static IQ readStanzaAsIq(String stanzaPath) throws IOException, DocumentException {
        String stanzaStr = IOUtils.toString(
                new FileInputStream(STANZA_PATH + stanzaPath));
        return toIq(stanzaStr);
    }

    public static IQ toIq(String stanzaStr) throws DocumentException {
        SAXReader xmlReader = new SAXReader();
        xmlReader.setMergeAdjacentText(true);
        xmlReader.setStringInternEnabled(true);
        xmlReader.setStripWhitespaceText(true);
        Element entry = xmlReader.read(new StringReader(stanzaStr)).getRootElement();
        
        return new IQ(entry);
    }
    
    public void testFeatureNotImplementedSuccess() throws IOException, InterruptedException, DocumentException {
        
        IQ request = readStanzaAsIq("/iq/featureNotImplemented/request.stanza");
        String expectedReply = readStanzaAsString("/iq/featureNotImplemented/reply.stanza");
        
        LinkedBlockingQueue<Packet> outQueue = new LinkedBlockingQueue<Packet>();
        LinkedBlockingQueue<Packet> inQueue = new LinkedBlockingQueue<Packet>();
        InQueueConsumer consumer = new InQueueConsumer(outQueue, IQHandlerTest.readConf(), inQueue);
        consumer.start();
        
        inQueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
}
