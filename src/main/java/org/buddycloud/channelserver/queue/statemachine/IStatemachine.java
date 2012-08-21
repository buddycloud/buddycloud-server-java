package org.buddycloud.channelserver.queue.statemachine;

import org.buddycloud.channelserver.db.DataStoreException;
import org.xmpp.packet.Packet;

public interface IStatemachine {

    public final static String KEY_EVENT_TYPE = "type";
    public final static String KEY_STATE = "state";
    public final static String KEY_ORIGINAL_REQUEST = "original_request";
    public final static String KEY_NODE = "node";

    public final static String STATE_INIT = "init";
    
    public Packet nextStep() throws DataStoreException;
}
