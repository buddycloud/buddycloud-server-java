package org.buddycloud.channels.queue;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.LinkedBlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channels.packetHandler.IPacketHandler;
import org.buddycloud.channels.packetHandler.IQ.IQHandler;
import org.xmpp.packet.Packet;

import redis.clients.jedis.Jedis;

public class InQueue {

	private OutQueue outQueue = null;
	private ErrorQueue errorQueue = null;
	
	protected LinkedBlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	
	Thread[] consumers = new Thread[3];
	
	private Properties conf;
	
	public InQueue(OutQueue outQueue, ErrorQueue errorQueue, Properties conf) {
		
		this.outQueue = outQueue;
		this.errorQueue = errorQueue;
		this.conf = conf;
		
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
	
	private class Consumer implements Runnable {
		
		private Logger LOGGER = Logger.getLogger(Consumer.class);
		
		private static final String IQ_PACKET_TYPE 			= "org.xmpp.packet.IQ";
		private static final String PRESENCE_PACKET_TYPE 	= "org.xmpp.packet.Presence";
		private static final String MESSAGE_PACKET_TYPE 	= "org.xmpp.packet.Message";
		
		private Map <String, IPacketHandler> packetHandlers = new HashMap<String, IPacketHandler>();
		
		private Jedis jedis;
		
		public Consumer() {
			
			this.jedis = new Jedis(conf.getProperty("redis.host"), 
								   Integer.valueOf(conf.getProperty("redis.port")));
			this.jedis.configSet("timeout", "0");
			
			//	this.packetHandlers.put(PRESENCE_PACKET_TYPE, new PresenceHandler());
			this.packetHandlers.put(IQ_PACKET_TYPE, new IQHandler(outQueue, errorQueue, this.jedis));
		}
		
		@Override
		public void run() {
			
			Long start;
			
			while (true) {
				try {
					
					Packet p = queue.take();
					
					start = System.currentTimeMillis();

					LOGGER.debug("IN -> " + p.toXML());
					
					if( this.packetHandlers.get(p.getClass().getName()) != null ) {
						packetHandlers.get(p.getClass().getName()).ingestPacket(p);
					} else {
						LOGGER.debug("Packet was not handled in any way.");
					}
					
					LOGGER.debug("Packet handled in '" + Long.toString((System.currentTimeMillis() - start)) + "' milliseconds.");
					
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
