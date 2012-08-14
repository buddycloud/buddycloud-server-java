package org.buddycloud.channelserver.packetprocessor.iq.namespace.register;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;


import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.db.DataStore;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class RegisterSet implements PacketProcessor<IQ> {

    public static final String ELEMENT_NAME = "query";
    
    private final Properties conf;
    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    public RegisterSet(Properties conf, BlockingQueue<Packet> outQueue,
            DataStore dataStore) {
        this.conf = conf;
        this.outQueue = outQueue;
        this.dataStore = dataStore;
    }

    @Override
    public void process(IQ reqIQ) throws Exception {
        
        String domain  = reqIQ.getFrom().getDomain();
        if(!domain.equals(conf.getProperty("server.domain"))) {
            
            // Request is coming from different domain than the
            // component is using. We will not allow this because
            // "buddycloud federation" cannot work for that.
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            reply.setChildElement(reqIQ.getChildElement().createCopy());
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.not_allowed, 
                                             org.xmpp.packet.PacketError.Type.cancel);
            reply.setError(pe);
            
            outQueue.put(reply);
            return;
        }
        
        String bareJID = reqIQ.getFrom().toBareJID();
        if(dataStore.addLocalUser(bareJID) == 0) {
            
            // User is already registered.
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setType(Type.error);
            reply.setChildElement(reqIQ.getChildElement().createCopy());
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.conflict, 
                                             org.xmpp.packet.PacketError.Type.cancel);
            reply.setError(pe);
            
            outQueue.put(reply);
            return;
        }
        
        //dataStore.createUserPostsNode(reqIQ.getFrom().toBareJID());
        dataStore.createUserNodes(reqIQ.getFrom().toBareJID());
        
        IQ result = IQ.createResultIQ(reqIQ);
        outQueue.put(result);
    }
    
}
