package org.buddycloud.channelserver.queue;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;
import org.xmpp.component.ComponentException;
import org.xmpp.packet.Packet;

public class OutQueueConsumer extends QueueConsumer {

    private static final Logger logger = Logger.getLogger(OutQueueConsumer.class);
    private final ChannelsEngine component;
	private FederatedQueueManager federatedQueue;
	private String server;
     
    public OutQueueConsumer(ChannelsEngine component, 
            BlockingQueue<Packet> outQueue, FederatedQueueManager federatedQueue, String server) {
        super(outQueue);
        this.component = component;
        this.federatedQueue = federatedQueue;
        this.server = server;
    }

	@Override
    protected void consume(Packet p) {
        try {
        	if ((-1 == p.getTo().toString().indexOf("@")) 
        	    && (p.getTo().toBareJID().indexOf(server) == -1)) {
        		// i.e. a remote server
        		federatedQueue.process(p);
        		return;
        	}
            component.sendPacket(p);
            logger.debug("OUT -> " + p.toXML());
        } catch (ComponentException e) {
            logger.error(e);
        }
    }
}