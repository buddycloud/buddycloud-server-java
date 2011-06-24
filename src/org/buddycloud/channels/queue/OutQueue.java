package org.buddycloud.channels.queue;

import java.util.concurrent.LinkedBlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channels.ChannelsEngine;
import org.xmpp.component.ComponentException;
import org.xmpp.component.ComponentManager;
import org.xmpp.packet.Packet;

public class OutQueue implements IOutQueue {

	private ComponentManager manager = null;
	private ChannelsEngine component      = null;
	
	protected LinkedBlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	
	Thread[] consumers = new Thread[3];
	
	public OutQueue(ComponentManager manager, ChannelsEngine component, boolean startConsumers) {
		
		this.manager = manager;
		this.component = component;
		
		if(!startConsumers) {
			return;
		}
		
		for (int i = 0; i < consumers.length; i++) {
			this.consumers[i] = new Thread(new Consumer());
			this.consumers[i].start();
		}
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
	
	private class Consumer implements Runnable {
		
		private Logger LOGGER = Logger.getLogger(Consumer.class);
		
		public Consumer() {
			
		}
		
		@Override
		public void run() {
			
			String componentJID = component.getJID().toBareJID();
			
			while (true) {
				try {
					Packet p = queue.take();
					try {
						
						p.setFrom(componentJID);
						
						if(p.getFrom().equals(p.getTo())) {
							// Sender and the receiver are the same! Skipping!
							// This is most likely a useless step.
							continue;
						}
						
						manager.sendPacket(component, p);
						
						LOGGER.debug("OUT -> " + p.toXML());
						System.out.println("OUT -> " + p.toXML());
						
					} catch (ComponentException e) {
						e.printStackTrace();
					}
					
				} catch (InterruptedException e) {
					//LogMe.warning("Error consuming OutQueue: '" + e.getMessage() + "'!");
					e.printStackTrace();
				} catch (Exception e) {
					//LogMe.warning("Error consuming OutQueue: '" + e.getMessage() + "'!");
					e.printStackTrace();
				}
			}
		}
		
	} // end of Consumer
	
}
