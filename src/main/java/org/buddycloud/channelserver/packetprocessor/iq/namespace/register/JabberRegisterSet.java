package org.buddycloud.channelserver.packetprocessor.iq.namespace.register;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class JabberRegisterSet implements PacketProcessor<IQ> {

	private RegisterSet registerSet;
	private UnregisterSet unregisterSet;

	public JabberRegisterSet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
		this.registerSet = new RegisterSet(outQueue, channelManager);
		this.unregisterSet = new UnregisterSet(outQueue, channelManager);
	}
	
	@Override
	public void process(IQ reqIQ) throws Exception {
		Element removeEl = reqIQ.getElement().element("query").element("remove");
		if (removeEl == null) {
			registerSet.process(reqIQ);
		} else {
			unregisterSet.process(reqIQ);
		}
	}
}
