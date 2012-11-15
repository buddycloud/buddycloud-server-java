package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoitems;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.AbstractNamespace;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class JabberDiscoItems extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/disco#items";

	private final PacketProcessor<IQ> resultProcessor;

	private FederatedQueueManager federatedQueueManager;

	public JabberDiscoItems(BlockingQueue<Packet> outQueue, Properties conf,
			ChannelManager channelManager,
			FederatedQueueManager federatedQueueManager) {
		super(outQueue, conf, channelManager);
		this.federatedQueueManager = federatedQueueManager;
		this.resultProcessor = new DiscoResult(outQueue, federatedQueueManager);
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
		return resultProcessor;
	}

	@Override
	protected PacketProcessor<IQ> error() {
		return null;
	}
}