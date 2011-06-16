package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.HashMap;
import java.util.Map;

import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

import redis.clients.jedis.Jedis;

abstract class AbstractNamespace implements INamespace {
	
	protected Map <String, IAction> setProcessors = new HashMap<String, IAction>();
	protected Map <String, IAction> getProcessors = new HashMap<String, IAction>();
	protected Map <String, IAction> resultProcessors = new HashMap<String, IAction>();
	
	public IQ reqIQ;
	
	public OutQueue outQueue;
	public ErrorQueue errorQueue;
	
	protected Jedis jedis;
	
	public AbstractNamespace(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		this.outQueue   = outQueue;
		this.errorQueue = errorQueue;
		this.jedis      = jedis;
	}
	
	public void ingestPacket(Packet p) {
		
		this.reqIQ = (IQ)p;
		
		//LogMe.debug("Packet is handled by '" + this.getClass().getName() + "'");
		
		if(reqIQ.getType() == IQ.Type.get && getProcessors.get(reqIQ.getChildElement().getName()) != null) {

			getProcessors.get(reqIQ.getChildElement().getName()).process();

		} else if(reqIQ.getType() == IQ.Type.set && setProcessors.get(reqIQ.getChildElement().getName()) != null) {
			
			setProcessors.get(reqIQ.getChildElement().getName()).process();
	
		} else if(reqIQ.getType() == IQ.Type.result && resultProcessors.get(reqIQ.getChildElement().getName()) != null) {
			
			resultProcessors.get(reqIQ.getChildElement().getName()).process();
			
		} else if(reqIQ.getType() == IQ.Type.error) {
			
			System.out.println("Errors not yet handled for namespace '" + this.getClass().getName() + "'.");
		
		} else if(reqIQ.getType() == IQ.Type.result) {
			
			System.out.println("This result was not handled in any way: '" + this.reqIQ.toXML() + "'.");
		
	    } else {
			
			ErrorPacket ep = ErrorPacketBuilder.unexpectedRequest(this.reqIQ);
			this.errorQueue.put(ep);
		
		}
		
	}
}
