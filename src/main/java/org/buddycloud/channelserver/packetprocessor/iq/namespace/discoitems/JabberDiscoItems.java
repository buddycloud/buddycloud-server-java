package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoitems;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.AbstractNamespace;
import org.dom4j.Namespace;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class JabberDiscoItems extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/disco#items";
	public static final Namespace NAMESPACE = Namespace.get(NAMESPACE_URI);
	
	public JabberDiscoItems(BlockingQueue<Packet> outQueue, Properties conf, ChannelManager channelManager) {
		super(outQueue, conf, channelManager);
	}

    @Override
    protected PacketProcessor<IQ> get() {
        return null;
    }

    @Override
    protected PacketProcessor<IQ> set() {
        return null;
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
