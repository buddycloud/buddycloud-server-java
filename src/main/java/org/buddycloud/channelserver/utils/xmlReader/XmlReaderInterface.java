package org.buddycloud.channelserver.utils.xmlReader;

import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.xmpp.packet.IQ;

public interface XmlReaderInterface
{
    public Element parse(IQ stanza) throws DocumentException;
}