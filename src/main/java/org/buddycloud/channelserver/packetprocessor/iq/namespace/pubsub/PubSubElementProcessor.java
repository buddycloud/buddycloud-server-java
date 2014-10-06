package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public interface PubSubElementProcessor {

    public static final int MAX_ITEMS_TO_RETURN = 50;

    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception;

    public boolean accept(Element elm);
}
