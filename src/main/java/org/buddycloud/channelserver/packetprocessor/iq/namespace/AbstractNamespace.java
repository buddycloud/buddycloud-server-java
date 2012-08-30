package org.buddycloud.channelserver.packetprocessor.iq.namespace;

import java.util.Map;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.queue.statemachine.IStatemachine;
import org.buddycloud.channelserver.queue.statemachine.StateMachineBuilder;

import org.apache.log4j.Logger;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public abstract class AbstractNamespace implements PacketProcessor<IQ> {
	
    private static final Logger LOGGER = Logger.getLogger(AbstractNamespace.class);
    
	private BlockingQueue<Packet> outQueue;
	private Properties conf;
	private DataStore dataStore;
	
	public AbstractNamespace(BlockingQueue<Packet> outQueue, 
	        Properties conf, DataStore dataStore) {
		this.outQueue  = outQueue;
		this.conf      = conf;
		this.dataStore = dataStore;
	}
	
	protected abstract PacketProcessor<IQ> get();
	
	protected abstract PacketProcessor<IQ> set();
	
	protected abstract PacketProcessor<IQ> result();
	
	protected abstract PacketProcessor<IQ> error();
	
	@Override
	public void process(IQ reqIQ) throws Exception {
	    
	    PacketProcessor<IQ> processor = null;
	    
        switch (reqIQ.getType()) {
        case get:
            processor = get();
            break;
        case set:
            processor = set();
            break;
        case result:
            processor = result();
            break;
        case error:
            processor = error();
            break;
        default:
            break;
        }
	    
	    if (processor != null) {
	        processor.process(reqIQ);
	        return;
	    }
	        
        if (reqIQ.getType() == IQ.Type.error || reqIQ.getType() == IQ.Type.result ) {
            handleStateReply(reqIQ);
        } else {
            handleUnexpectedRequest(reqIQ);
        }
	}

    private void handleUnexpectedRequest(IQ reqIQ) throws InterruptedException {
        IQ reply = IQ.createResultIQ(reqIQ);
        reply.setChildElement(reqIQ.getChildElement().createCopy());
        reply.setType(Type.error);
        PacketError pe = new PacketError(PacketError.Condition.unexpected_request, 
                PacketError.Type.wait);
        reply.setError(pe);
        
        outQueue.put(reply);
    }

    private void handleStateReply(IQ reqIQ) throws InterruptedException, DataStoreException {
        // This might be a reply to a state we are on.
        Map<String, String> state = dataStore.getState(reqIQ.getID());
        if (state != null && !state.isEmpty()) {
            IStatemachine sm = StateMachineBuilder.buildFromState(
                    reqIQ, state, dataStore);
            outQueue.put(sm.nextStep());
        } else {
            LOGGER.error("This result was not handled in any way: '" + reqIQ.toXML() + "'.");
        }
    }
    
    protected DataStore getDataStore() {
        return dataStore;
    }
    
    protected BlockingQueue<Packet> getOutQueue() {
        return outQueue;
    }
	
    public Properties getConf() {
        return conf;
    }
}
