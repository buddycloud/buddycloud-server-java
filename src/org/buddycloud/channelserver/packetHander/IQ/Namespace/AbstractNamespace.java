package org.buddycloud.channelserver.packetHander.IQ.Namespace;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.queue.AOutQueue;
import org.buddycloud.channelserver.queue.statemachine.IStatemachine;
import org.buddycloud.channelserver.queue.statemachine.StateMachineBuilder;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.PacketError;

abstract class AbstractNamespace implements INamespace {
	
	protected Map <String, IAction> setProcessors = new HashMap<String, IAction>();
	protected Map <String, IAction> getProcessors = new HashMap<String, IAction>();
	protected Map <String, IAction> resultProcessors = new HashMap<String, IAction>();
	protected Map <String, IAction> errorProcessors = new HashMap<String, IAction>();
	
	public IQ reqIQ;
	
	public AOutQueue outQueue;
	public Properties conf;
	public DataStore dataStore;
	
	public AbstractNamespace(AOutQueue outQueue, Properties conf, DataStore dataStore) {
		this.outQueue  = outQueue;
		this.conf      = conf;
		this.dataStore = dataStore;
	}
	
	public void ingestIQ(IQ iq) {
		
		this.reqIQ = iq;
		
		//LogMe.debug("Packet is handled by '" + this.getClass().getName() + "'");
		
		if(reqIQ.getType() == IQ.Type.get && getProcessors.get(reqIQ.getChildElement().getName()) != null) {

			getProcessors.get(reqIQ.getChildElement().getName()).process();

		} else if(reqIQ.getType() == IQ.Type.set && setProcessors.get(reqIQ.getChildElement().getName()) != null) {
			
			setProcessors.get(reqIQ.getChildElement().getName()).process();
	
		} else if(reqIQ.getType() == IQ.Type.result && resultProcessors.get(reqIQ.getChildElement().getName()) != null) {
			
			resultProcessors.get(reqIQ.getChildElement().getName()).process();
			
		} else if(reqIQ.getType() == IQ.Type.error && errorProcessors.get(reqIQ.getChildElement().getName()) != null) {
			
			errorProcessors.get(reqIQ.getChildElement().getName()).process();
		
		} else if(reqIQ.getType() == IQ.Type.error || reqIQ.getType() == IQ.Type.result ) {
			
		    // This might be a reply to a state we are on.
            HashMap<String, String> state = dataStore.getState(iq.getID());
            if(!state.isEmpty()) {
                IStatemachine sm = StateMachineBuilder.buildFromState(iq, state, dataStore);
                outQueue.put(sm.nextStep());
                return;
            }
		    
			System.out.println("This result was not handled in any way: '" + this.reqIQ.toXML() + "'.");
		
	    } else {
			
	        IQ reply = IQ.createResultIQ(iq);
            reply.setChildElement(iq.getChildElement().createCopy());
            reply.setType(Type.error);
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.unexpected_request, 
                                             org.xmpp.packet.PacketError.Type.wait);
            reply.setError(pe);
            
            this.outQueue.put(reply);
            
		}
		
	}
}
