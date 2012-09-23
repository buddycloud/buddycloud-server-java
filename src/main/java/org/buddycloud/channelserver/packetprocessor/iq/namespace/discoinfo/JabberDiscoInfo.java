package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.AbstractNamespace;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class JabberDiscoInfo extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/disco#info";
	
	private final PacketProcessor<IQ> getProcessor;
	
	public JabberDiscoInfo(BlockingQueue<Packet> outQueue, Properties conf, ChannelManager channelManager) {
		super(outQueue, conf, channelManager);
		this.getProcessor = new DiscoInfoGet(outQueue, channelManager);
	}

    @Override
    protected PacketProcessor<IQ> get() {
        return getProcessor;
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
