package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class Search implements PacketProcessor<IQ> {
	
	public static final String ELEMENT_NAME = "query";
	private static final Logger logger = Logger.getLogger(Search.class);
	
	public static final String NAMESPACE_URI = "jabber:iq:search";

	private final BlockingQueue<Packet> outQueue;
	private IQ request;

	private SearchGet searchGet;
	private SearchSet searchSet;

	public Search(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.searchGet = new SearchGet(outQueue);
		this.searchSet = new SearchSet(outQueue, channelManager);
	}

	@Override
	public void process(IQ reqIQ) throws Exception {
		request = reqIQ;
		if (request.getType().equals(Type.get)) {
			logger.trace("Using search processor: SearchGet");
			this.searchGet.process(request);
			return;
		} else if (request.getType().equals(Type.set)) {
			logger.trace("Using search processor: SearchSet");
			this.searchSet.process(request);
			return;
		}
		IQ response = IQ.createResultIQ(request);
		response.setType(IQ.Type.error);
		PacketError error = new PacketError(
				PacketError.Condition.bad_request,
				PacketError.Type.modify
		);
		response.setError(error);
		outQueue.put(response);
	}
}