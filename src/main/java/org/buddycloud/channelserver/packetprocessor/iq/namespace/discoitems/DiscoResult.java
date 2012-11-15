package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoitems;

import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo.DiscoInfoGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class DiscoResult implements PacketProcessor<IQ> {

	public static final String ELEMENT_NAME = "query";
	private static final Logger logger = Logger.getLogger(DiscoInfoGet.class);
	private final BlockingQueue<Packet> outQueue;
	private String node;

	private IQ requestIq;
	private FederatedQueueManager federatedQueueManager;

	public DiscoResult(BlockingQueue<Packet> outQueue,
			FederatedQueueManager federatedQueueManager) {
		this.outQueue = outQueue;
		this.federatedQueueManager = federatedQueueManager;
	}

	@Override
	public void process(IQ reqIQ) throws Exception {
	    this.requestIq = reqIQ;
	    List<Element> items = requestIq.getChildElement().elements("item");
	    federatedQueueManager.sendInfoRequests(requestIq.getFrom(), items);
	}
}