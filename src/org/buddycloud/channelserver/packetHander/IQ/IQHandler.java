package org.buddycloud.channelserver.packetHander.IQ;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetHander.APacketHandler;
import org.buddycloud.channelserver.packetHander.IQ.Namespace.INamespace;
import org.buddycloud.channelserver.packetHander.IQ.Namespace.JabberDiscoInfo;
import org.buddycloud.channelserver.packetHander.IQ.Namespace.JabberPubsub;
import org.buddycloud.channelserver.packetHander.IQ.Namespace.JabberRegister;
import org.buddycloud.channelserver.queue.AOutQueue;
import org.buddycloud.channelserver.queue.statemachine.IStatemachine;
import org.buddycloud.channelserver.queue.statemachine.StateMachineBuilder;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.PacketError;

public class IQHandler extends APacketHandler {

    private Logger LOGGER = Logger.getLogger(IQHandler.class);
    
	private Map <String, INamespace> namespaceHandlers = new HashMap<String, INamespace>();
	
	public IQHandler(AOutQueue outQueue, Properties conf, DataStore dataStore) {
		
		this.outQueue  = outQueue;
		this.dataStore = dataStore;
		
		namespaceHandlers.put(JabberDiscoInfo.NAMESPACE_URI, new JabberDiscoInfo(outQueue, conf, dataStore));
		namespaceHandlers.put(JabberRegister.NAMESPACE_URI,  new JabberRegister(outQueue, conf, dataStore));
		namespaceHandlers.put(JabberPubsub.NAMESPACE_URI,    new JabberPubsub(outQueue, conf, dataStore));

	}
	
	public void ingestIQ(IQ iq) {
		
        if( iq.getChildElement() != null && 
            iq.getChildElement().getNamespaceURI() != null && 
            namespaceHandlers.get(iq.getChildElement().getNamespaceURI()) != null ) {
          
                namespaceHandlers.get(iq.getChildElement().getNamespaceURI()).ingestIQ(iq);
                return;
                
        } 		    
	    
        if(iq.getType() == IQ.Type.set || iq.getType() == IQ.Type.get) {
    		
            IQ reply = IQ.createResultIQ(iq);
            reply.setChildElement(iq.getChildElement().createCopy());
            reply.setType(Type.error);
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.service_unavailable, 
                                             org.xmpp.packet.PacketError.Type.cancel);
            reply.setError(pe);
            
            this.outQueue.put(reply);
            return;
            
        }
		
        if(iq.getType() == IQ.Type.result || iq.getType() == IQ.Type.error) {
            
            // This might be a reply to a state we are on.
            HashMap<String, String> state = dataStore.getState(iq.getID());
            if(!state.isEmpty()) {
                IStatemachine sm = StateMachineBuilder.buildFromState(iq, state, dataStore);
                outQueue.put(sm.nextStep());
                return;
            }
            
        }
        
		LOGGER.info("DID NOT HANDLE THE IQ PACKET.");
		
//		if( iq.getChildElement() != null && 
//			iq.getChildElement().getNamespaceURI() != null && 
//			namespaceHandlers.get(iq.getChildElement().getNamespaceURI()) != null ) {
//			
//			namespaceHandlers.get(iq.getChildElement().getNamespaceURI()).ingestPacket(p);
//			return;
//			
//		} 
//		
//		if(iq.getType() == IQ.Type.error) {
//			System.out.println("NOT SO QUIETLY SKIPPING RECEIVED ERROR IQ without namespace:" + iq.toXML());
//			return;
//		} else if (iq.getType() == IQ.Type.result) {
//			System.out.println("NOT SO QUIETLY skipping result IQ's without handled namespaces." + iq.toXML());
//            return;
//		}
//		
//		if(iq.getChildElement() == null) {
//			//System.out.println("Received packet without a namespace: " + iq.toXML());
//		} else {
//			//System.out.println("'IQHandler' received IQ with childElement with namespace '" + iq.getChildElement().getNamespaceURI() + "' that we did not find a handler for!");
//			System.out.println("Something weird: " + iq.toXML());
//		}
//		
//		ErrorPacket ep = ErrorPacketBuilder.featureNotImplemented(iq);
//		this.errorQueue.put(ep);
	
	}

}
