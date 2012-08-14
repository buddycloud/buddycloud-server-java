package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;


import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.AbstractNamespace;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class JabberPubsub extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/pubsub";
	
	public static final String NS_XMPP_STANZAS = "urn:ietf:params:xml:ns:xmpp-stanzas";
    public static final String NS_PUBSUB_ERROR = "http://jabber.org/protocol/pubsub#errors";
    public static final String NS_PUBSUB_EVENT = "http://jabber.org/protocol/pubsub#event";
    public static final String NS_PUBSUB_OWNER = "http://jabber.org/protocol/pubsub#owner";

    private final PubSubGet getProcessor;
    private final PubSubSet setProcessor;
	
	public JabberPubsub(BlockingQueue<Packet> outQueue, Properties conf, DataStore dataStore) {
		
	    super(outQueue, conf, dataStore);
	    this.getProcessor = new PubSubGet(outQueue, dataStore);
	    this.setProcessor = new PubSubSet(outQueue, dataStore);
	}
	
    @Override
    protected PacketProcessor<IQ> get() {
        return getProcessor;
    }

    @Override
    protected PacketProcessor<IQ> set() {
        return setProcessor;
    }

    @Override
    protected PacketProcessor<IQ> result() {
        return null;
    }

    @Override
    protected PacketProcessor<IQ> error() {
        return null;
    }
	
}
