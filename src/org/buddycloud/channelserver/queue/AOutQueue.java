package org.buddycloud.channelserver.queue;

import java.util.concurrent.LinkedBlockingQueue;

import org.apache.log4j.Logger;
import org.xmpp.packet.Packet;

public abstract class AOutQueue {

    private Logger LOGGER = Logger.getLogger(AOutQueue.class);
    
    protected LinkedBlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
    
    public AOutQueue() {
        
    }

    public void put(Packet p) {
        try {
            this.queue.put(p);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
    
    public LinkedBlockingQueue<Packet> getQueue() {
        return this.queue;
    }
    
}
