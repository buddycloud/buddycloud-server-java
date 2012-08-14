package org.buddycloud.channelserver.queue;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;
import org.xmpp.component.ComponentException;
import org.xmpp.packet.Packet;

public class OutQueueConsumer extends QueueConsumer {

    private static final Logger LOGGER = Logger.getLogger(OutQueueConsumer.class);
    private final ChannelsEngine component;
    
    public OutQueueConsumer(ChannelsEngine component, 
            BlockingQueue<Packet> outQueue) {
        super(outQueue);
        this.component = component;
    }

    @Override
    protected void consume(Packet p) {
        try {
            component.sendPacket(p);
            LOGGER.debug("OUT -> " + p.toXML());
        } catch (ComponentException e) {
            LOGGER.error(e);
        }
    }

}
