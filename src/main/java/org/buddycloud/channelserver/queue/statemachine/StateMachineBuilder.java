package org.buddycloud.channelserver.queue.statemachine;

import java.util.Map;

import org.buddycloud.channelserver.db.DataStore;

import org.apache.log4j.Logger;
import org.xmpp.packet.IQ;

public class StateMachineBuilder {

    protected static Logger LOGGER = Logger.getLogger(StateMachineBuilder.class);
    
    public static IStatemachine buildFromState(IQ iq, Map<String, String> state, DataStore dataStore) {
        
        LOGGER.debug("Building a state for event type '" +  state.get(IStatemachine.KEY_EVENT_TYPE) +"'.");
        if(state.get(IStatemachine.KEY_EVENT_TYPE).equals(Subscribe.EVENT_TYPE)) {
            return Subscribe.buildFromState(iq, state, dataStore);
        } else if(state.get(IStatemachine.KEY_EVENT_TYPE).equals(Unsubscribe.EVENT_TYPE)) {
            return Unsubscribe.buildFromState(iq, state, dataStore);
        } else if(state.get(IStatemachine.KEY_EVENT_TYPE).equals(DiscoInfo.EVENT_TYPE)) {
            return DiscoInfo.buildFromState(iq, state, dataStore);
        } else if(state.get(IStatemachine.KEY_EVENT_TYPE).equals(Publish.EVENT_TYPE)) {
            return Publish.buildFromState(iq, state, dataStore);
        } 
        
        LOGGER.error("Tried to create a event that is not configured: '" + state.get(IStatemachine.KEY_EVENT_TYPE) + 
                     "'! This means that this thing does not work! FIX IT FIX IT FIX IT!");
        
        return null;
    }
    
    
}
