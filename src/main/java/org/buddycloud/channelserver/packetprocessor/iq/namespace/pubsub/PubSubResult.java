package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.BlockingQueue;


import org.apache.log4j.Logger;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.AffiliationsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.ItemsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.SubscriptionsGet;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.buddycloud.channelserver.queue.UnknownFederatedPacketException;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class PubSubResult implements PacketProcessor<IQ> {

    public static final String ELEMENT_NAME = "pubsub";   
    private Logger logger = Logger.getLogger(PubSubResult.class);    
    private final BlockingQueue<Packet> outQueue;
	private FederatedQueueManager federatedQueueManager;
    
    public PubSubResult(BlockingQueue<Packet> outQueue, FederatedQueueManager federatedQueueManager) {
        this.outQueue = outQueue;
        this.federatedQueueManager = federatedQueueManager;
    }
    
    @Override
    public void process(IQ reqIQ) throws Exception {
    	try {
		    federatedQueueManager.passResponseToRequester(reqIQ);
		    return;
		} catch (UnknownFederatedPacketException e) {
			logger.error(e);
		}
    	
        IQ reply = IQ.createResultIQ(reqIQ);
        reply.setChildElement(reqIQ.getChildElement().createCopy());
        reply.setType(Type.error);
        PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.unexpected_request, 
                                         org.xmpp.packet.PacketError.Type.wait);
        reply.setError(pe);
        outQueue.put(reply);
    }
}
