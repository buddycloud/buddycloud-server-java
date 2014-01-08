package org.buddycloud.channelserver.packetHandler.iq;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.StringReader;

import org.apache.commons.io.IOUtils;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;

public class TestHandler {
	
    public final static String  STANZA_PATH = "src/test/resources/stanzas";
    
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
}