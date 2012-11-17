package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class ItemsProcessor implements PacketProcessor<Message> {

	public ItemsProcessor(BlockingQueue<Packet> outQueue,
			Properties configuration, ChannelManager channelManager) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void process(Message packet) throws Exception {
		// TODO Auto-generated method stub

	}

}
