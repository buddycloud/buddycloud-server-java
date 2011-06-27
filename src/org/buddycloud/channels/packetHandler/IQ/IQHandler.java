package org.buddycloud.channels.packetHandler.IQ;

import java.util.HashMap;
import java.util.Map;

import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.packetHandler.APacketHandler;
import org.buddycloud.channels.packetHandler.IPacketHandler;
import org.buddycloud.channels.packetHandler.IQ.Namespace.INamespace;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoInfo;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoItems;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsub;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsubOwner;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberRegister;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

import redis.clients.jedis.Jedis;

public class IQHandler extends APacketHandler implements IPacketHandler {

	private Map <String, INamespace> namespaceHandlers = new HashMap<String, INamespace>();
	
	public IQHandler(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		this.outQueue   = outQueue;
		this.errorQueue = errorQueue;
		this.jedis      = jedis;
		
		namespaceHandlers.put(JabberRegister.NAMESPACE_URI, new JabberRegister(outQueue, 
																			   errorQueue, 
																			   this.jedis));
		namespaceHandlers.put(JabberPubsub.NAMESPACE_URI, new JabberPubsub(outQueue, 
																		   errorQueue, 
																		   this.jedis));
		namespaceHandlers.put(JabberPubsubOwner.NAMESPACE_URI, new JabberPubsubOwner(outQueue, 
																		   			 errorQueue, 
																		   			 this.jedis));
		namespaceHandlers.put(JabberDiscoItems.NAMESPACE_URI, new JabberDiscoItems(outQueue, 
																			       errorQueue, 
																			       this.jedis));
		namespaceHandlers.put(JabberDiscoInfo.NAMESPACE_URI, new JabberDiscoInfo(outQueue, 
				   							   							         errorQueue, 
				   														         this.jedis));
	}
	
	@Override
	public void ingestPacket(Packet p) {
		
		IQ iq = (IQ)p;
		
		if( iq.getChildElement() != null && 
			iq.getChildElement().getNamespaceURI() != null && 
			namespaceHandlers.get(iq.getChildElement().getNamespaceURI()) != null ) {
			
			namespaceHandlers.get(iq.getChildElement().getNamespaceURI()).ingestPacket(p);
			return;
			
		} 
		
		if(iq.getType() == IQ.Type.error) {
			System.out.println("NOT SO QUIETLY SKIPPING RECEIVED ERROR IQ without namespace:" + iq.toXML());
			return;
		} else if (iq.getType() == IQ.Type.result) {
			System.out.println("NOT SO QUIETLY skipping result IQ's without handled namespaces." + iq.toXML());
            return;
		}
		
		if(iq.getChildElement() == null) {
			//System.out.println("Received packet without a namespace: " + iq.toXML());
		} else {
			//System.out.println("'IQHandler' received IQ with childElement with namespace '" + iq.getChildElement().getNamespaceURI() + "' that we did not find a handler for!");
			System.out.println("Something weird: " + iq.toXML());
		}
		
		ErrorPacket ep = ErrorPacketBuilder.featureNotImplemented(iq);
		this.errorQueue.put(ep);
	
	}

}
