package org.buddycloud.channelserver.packetHandler.iq;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.net.UnknownHostException;
import java.sql.SQLException;
import java.util.Properties;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.Main;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.channel.ChannelManagerFactoryImpl;
import org.buddycloud.channelserver.channel.ChannelManagerImpl;
import org.buddycloud.channelserver.channel.TestHelper;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.NodeStoreFactory;
import org.buddycloud.channelserver.db.jdbc.DatabaseTester;
import org.buddycloud.channelserver.db.jdbc.JDBCNodeStore;
import org.buddycloud.channelserver.db.jdbc.dialect.Sql92NodeStoreDialect;
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

public class IQTestHandler extends TestCase
{
	private final static String CONFIGURATION_PROPERTIES = "src/test/resources/configuration.properties";
    public final static String  STANZA_PATH              = "src/test/resources/stanzas";
    public final static String  LOGGER_PROPERTIES        = "src/test/resources/log4j.properties";
    
    public static Properties readConf() 
        throws FileNotFoundException, IOException
    {
        Configuration conf = Configuration.getInstance();
        try {
            conf.load(new FileInputStream(CONFIGURATION_PROPERTIES));
        } catch (IOException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
        return conf;
    }
    
    public static String readStanzaAsString(String stanzaPath) 
        throws IOException, DocumentException
    {
        String stanzaStr = IOUtils.toString(
                new FileInputStream(STANZA_PATH + stanzaPath));
        return stanzaStr.replaceAll("   ", "").replaceAll("\n", "");
    }
    
    public static IQ readStanzaAsIq(String stanzaPath)
        throws IOException, DocumentException
    {
        String stanzaStr = IOUtils.toString(
                new FileInputStream(STANZA_PATH + stanzaPath));
        return toIq(stanzaStr);
    }

    public static IQ toIq(String stanzaStr) throws DocumentException
    {
        return new IQ(parseXml(stanzaStr));
    }
    
    public static Element parseXml(String stanzaStr) throws DocumentException {
    	SAXReader xmlReader = new SAXReader();
        xmlReader.setMergeAdjacentText(true);
        xmlReader.setStringInternEnabled(true);
        xmlReader.setStripWhitespaceText(true);
        return xmlReader.read(new StringReader(stanzaStr)).getRootElement();
	}

	public void featureNotImplementedSuccess()
        throws IOException, InterruptedException, DocumentException
    {    
        IQ request = readStanzaAsIq("/iq/featureNotImplemented/request.stanza");
        String expectedReply = readStanzaAsString("/iq/featureNotImplemented/reply.stanza");
        
        TestHelper helper = new TestHelper();
        
        
        helper.getInQueue().put(request);
        
        IQ replyIQ = (IQ)helper.getOutQueue().poll(1000, TimeUnit.MILLISECONDS);
        
        assertNotNull(replyIQ);
        assertEquals(expectedReply, replyIQ.toXML());
    }
}
