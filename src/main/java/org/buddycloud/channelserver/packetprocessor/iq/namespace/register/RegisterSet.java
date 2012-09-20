package org.buddycloud.channelserver.packetprocessor.iq.namespace.register;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;


import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.db.DataStore;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class RegisterSet implements PacketProcessor<IQ>
{
    public static final String ELEMENT_NAME = "query";
    
    private final Properties            conf;
    private final BlockingQueue<Packet> outQueue;
    private final DataStore             dataStore;
    private       IQ                    request;
    
    public RegisterSet(Properties conf, BlockingQueue<Packet> outQueue,
        DataStore dataStore)
    {
        this.conf      = conf;
        this.outQueue  = outQueue;
        this.dataStore = dataStore;
    }

    @Override
    public void process(IQ reqIQ) throws Exception
    {   
    	request = reqIQ;
    	
        String domain = request.getFrom().getDomain();
        if (!domain.equals(conf.getProperty("server.domain"))) {
            notThisDomain();
            return;
        }
        
        String bareJid = request.getFrom().toBareJID();
        if (true == dataStore.isLocalUser(bareJid)) {
            //userAlreadyRegistered();
        	IQ reply = IQ.createResultIQ(request);
            outQueue.put(reply);
            return;
        }
        dataStore.addLocalUser(bareJid);
        dataStore.createUserNodes(reqIQ.getFrom().toBareJID());
        IQ result = IQ.createResultIQ(reqIQ);
        outQueue.put(result);
    }

	private void userAlreadyRegistered() throws InterruptedException
	{
		// User is already registered.
        IQ reply = IQ.createResultIQ(request);
        reply.setType(Type.error);
        reply.setChildElement(request.getChildElement().createCopy());
        PacketError pe = new PacketError(
            org.xmpp.packet.PacketError.Condition.conflict, 
            org.xmpp.packet.PacketError.Type.cancel
        );
        reply.setError(pe);
        outQueue.put(reply);
	}

	private void notThisDomain() throws InterruptedException
	{
        // Request is coming from different domain than the
        // component is using. We will not allow this because
        // "buddycloud federation" cannot work for that.
        
        IQ reply = IQ.createResultIQ(request);
        reply.setType(Type.error);
        reply.setChildElement(request.getChildElement().createCopy());
        PacketError pe = new PacketError(
            org.xmpp.packet.PacketError.Condition.not_allowed, 
            org.xmpp.packet.PacketError.Type.cancel
        );
        reply.setError(pe);
        outQueue.put(reply);
	}
}