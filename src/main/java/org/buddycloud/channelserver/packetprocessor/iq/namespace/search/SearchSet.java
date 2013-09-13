package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;


public class SearchSet implements PacketProcessor<IQ> {

	private ChannelManager channelManager;
	private BlockingQueue<Packet> outQueue;

	public SearchSet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
		this.channelManager = channelManager;
		this.outQueue = outQueue;
	}
	
	@Override
	public void process(IQ packet) throws Exception {

	}

}
