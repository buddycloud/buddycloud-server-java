package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoitems;

import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class DiscoResult implements PacketProcessor<IQ> {

    public static final String ELEMENT_NAME = "query";
    private IQ requestIq;
    private FederatedQueueManager federatedQueueManager;

    public DiscoResult(BlockingQueue<Packet> outQueue, FederatedQueueManager federatedQueueManager) {
        this.federatedQueueManager = federatedQueueManager;
    }

    @Override
    public void process(IQ reqIQ) throws Exception {
        this.requestIq = reqIQ;
        List<Element> items = requestIq.getChildElement().elements(XMLConstants.ITEM_ELEM);
        federatedQueueManager.processDiscoItemsResponse(requestIq.getFrom(), items);
    }
}
