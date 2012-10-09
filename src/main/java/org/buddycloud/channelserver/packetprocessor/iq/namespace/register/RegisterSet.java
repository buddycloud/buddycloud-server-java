package org.buddycloud.channelserver.packetprocessor.iq.namespace.register;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;


import org.apache.log4j.Logger;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class RegisterSet implements PacketProcessor<IQ>
{
    public static final String ELEMENT_NAME = "query";
    private static final Logger LOGGER = Logger.getLogger(RegisterSet.class);
    
    private final Properties            conf;
    private final BlockingQueue<Packet> outQueue;
    private final ChannelManager             channelManager;
    private       IQ                    request;
    
    public RegisterSet(Properties conf, BlockingQueue<Packet> outQueue,
        ChannelManager channelManager)
    {
        this.conf      = conf;
        this.outQueue  = outQueue;
        this.channelManager = channelManager;
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
        LOGGER.trace("Processing register request from " + request.getFrom());
        if (true == channelManager.nodeExists("/user/" + request.getFrom() + "/posts")) {
            //userAlreadyRegistered();
        	LOGGER.trace("User " + request.getFrom() + " is already registered");
        	IQ reply = IQ.createResultIQ(request);
            outQueue.put(reply);
            return;
        }
        LOGGER.trace("Registering new user " + request.getFrom());
        channelManager.createPersonalChannel(reqIQ.getFrom());
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