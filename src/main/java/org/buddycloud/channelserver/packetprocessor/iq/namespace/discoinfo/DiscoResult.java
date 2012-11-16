package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo;

import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
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
	    requestIq = reqIQ;
	    String node = requestIq.getElement().element("query").attributeValue("node");
	    if (null == node) {
	        List<Element> identities = requestIq.getChildElement().elements("identity");
	        federatedQueueManager.processInfoResponses(requestIq.getFrom(), requestIq.getID(), identities);
	        return;
	    }
	    federatedQueueManager.passResponseToRequester(requestIq);
	}
}