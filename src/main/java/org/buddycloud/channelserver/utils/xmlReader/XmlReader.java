package org.buddycloud.channelserver.utils.xmlReader;

import java.io.StringReader;

import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;

public class XmlReader implements XmlReaderInterface
{
	protected SAXReader xmlReader;

	public XmlReader()
	{
		xmlReader = new SAXReader();
        xmlReader.setMergeAdjacentText(true);
        xmlReader.setStringInternEnabled(true);
        xmlReader.setStripWhitespaceText(true);
	}

	public Element parse(IQ stanza) throws DocumentException
	{
        return xmlReader.read(new StringReader(stanza.toString()))
            .getRootElement();
	}
}