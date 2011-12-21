package org.buddycloud.channelserver.packetHandler.IQ;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.UnknownHostException;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import org.buddycloud.channelserver.queue.InQueue;
import org.buddycloud.channelserver.queue.TestOutQueue;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;

import redis.clients.jedis.Jedis;

import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.Mongo;
import com.mongodb.MongoException;

public class IQHandlerTest extends TestCase {

    public final static String STANZA_PATH = "/home/tkoski/dev/java-workspace/channel-server-java/tests/resources/stanzas";
    
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
            conf.load(new FileInputStream("/home/tkoski/dev/java-workspace/channel-server-java/tests/resources/configuration.properties"));
        } catch (IOException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
        return conf;
    }
    
    public static String readStanzaAsString(String stanza) throws IOException, DocumentException {
        
        FileInputStream fstream = new FileInputStream(STANZA_PATH + stanza);
        
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
        
        return sb.toString().replaceAll("   ", "");
        
    }
    
    public static IQ readStanzaAsIq(String stanza) throws IOException, DocumentException {
        
        FileInputStream fstream = new FileInputStream(STANZA_PATH + stanza);
        
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
        
        return new IQ(entry);
        
    }
    
    public void testFeatureNotImplementedSuccess() throws IOException, InterruptedException, DocumentException {
        
        String request   = readStanzaAsString("/iq/featureNotImplemented/request.stanza");
        String expectedReply = readStanzaAsString("/iq/featureNotImplemented/reply.stanza");
        
        TestOutQueue outQueue = new TestOutQueue();
        InQueue inqueue = new InQueue(outQueue, readConf());
        
        inqueue.put(request);
        
        IQ replyIQ = (IQ)outQueue.getQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
    
}
