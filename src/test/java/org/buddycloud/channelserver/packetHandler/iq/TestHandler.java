package org.buddycloud.channelserver.packetHandler.iq;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.buddycloud.channelserver.Configuration;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;

public class TestHandler {
    
    public static final String  STANZA_PATH = "src/test/resources/stanzas";
    
    private static final String CONFIGURATION_PROPERTIES = "src/test/resources/configuration.properties";
    public static final String  LOGGER_PROPERTIES        = "src/test/resources/log4j.properties";
    
    public static Properties readConf() 
        throws FileNotFoundException, IOException {
        Configuration.reset();
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
        throws IOException, DocumentException {
        String stanzaStr = IOUtils.toString(
                new FileInputStream(STANZA_PATH + stanzaPath));
        return stanzaStr.replaceAll("   ", "").replaceAll("\n", "");
    }
    
    public static IQ readStanzaAsIq(String stanzaPath)
        throws IOException, DocumentException {
        String stanzaStr = IOUtils.toString(
                new FileInputStream(STANZA_PATH + stanzaPath));
        return toIq(stanzaStr);
    }

    public static IQ toIq(String stanzaStr) throws DocumentException {
        return new IQ(parseXml(stanzaStr));
    }
    
    public static Element parseXml(String stanzaStr) throws DocumentException {
        SAXReader xmlReader = new SAXReader();
        xmlReader.setMergeAdjacentText(true);
        xmlReader.setStringInternEnabled(true);
        xmlReader.setStripWhitespaceText(true);
        return xmlReader.read(new StringReader(stanzaStr)).getRootElement();
    }
}
