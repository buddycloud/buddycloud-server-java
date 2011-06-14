package org.buddycloud.channels.queue;

import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channels.packet.ErrorPacket;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;

public class ErrorQueue {

	private OutQueue outQueue = null;
	
	private LinkedBlockingQueue<ErrorPacket> queue = new LinkedBlockingQueue<ErrorPacket>();
	
	Thread[] consumers = new Thread[1];
	
	public ErrorQueue(OutQueue outQueue) {
		this.outQueue = outQueue;
		
		for (int i = 0; i < consumers.length; i++) {
			this.consumers[i] = new Thread(new Consumer());
			this.consumers[i].start();
		}
	}
	
	public void put(ErrorPacket ep) {
		try {
			this.queue.put(ep);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
	
	public OutQueue getOutQueue() {
		return this.outQueue;
	}
	
	private class Consumer implements Runnable {
		
		public Consumer() {
		}
		
		@Override
		public void run() {
			
			Long start;
			
			while (true) {
				try {
					
					ErrorPacket p = queue.take();
					
					//System.out.println("Got packet.");
					
					start = System.currentTimeMillis();
					
					if(p == null) {
						return;
					}
				
					IQ result = IQ.createResultIQ(p.getOriginalIQ());
					result.setType(Type.error);
					
					Element error = new DOMElement("error");
					error.addAttribute("type", p.getType());
					
					for (Element elm : p.getConditions()) {
						error.add(elm);						
					}
					
					result.setChildElement(error);
					
					//System.out.println("Error reason: '" + p.getMsg() + "'.");
					
					outQueue.put( result );
					
					//System.out.println("Outgoing error packed handled '" + Long.toString((System.currentTimeMillis() - start)) + "' milliseconds.");
					
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
