package org.buddycloud.channelserver.queue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;
import org.xmpp.component.ComponentException;
import org.xmpp.component.ComponentManager;
import org.xmpp.packet.Packet;

public class OutQueue extends AOutQueue {

    Thread[] consumers;
    
    private ComponentManager manager = null;
    private ChannelsEngine component = null;
    
    public OutQueue() {
        
        this.consumers = new Thread[1];
        
        for (int i = 0; i < consumers.length; i++) {
            this.consumers[i] = new Thread(new Consumer());
            this.consumers[i].start();
        }
        
    }
    
    public void setChannelsEngine(ComponentManager manager) {
        this.manager = manager;
    }
    
    public void setChannelsEngine(ChannelsEngine component) {
        this.component = component;
    }
    
    private class Consumer implements Runnable {
        
        private Logger LOGGER = Logger.getLogger(Consumer.class);
        
        public void run() {
            
            String componentJID = component.getJID().getDomain();
            
            while (true) {
                try {
                    Packet p = queue.take();
                    try {
                        
                        p.setFrom(componentJID);
                        
                        manager.sendPacket(component, p);
                        
                        LOGGER.debug("OUT -> " + p.toXML());
                        
                    } catch (ComponentException e) {
                        e.printStackTrace();
                    }
                    
                } catch (InterruptedException e) {
                    e.printStackTrace();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        
    }
}
