package org.buddycloud.channelserver.queue;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.connection.ComponentXMPPConnection;
import org.buddycloud.channelserver.connection.PacketReceiver;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.IQProcessor;
import org.buddycloud.channelserver.packetprocessor.message.MessageProcessor;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class InQueueConsumer extends QueueConsumer {

    private static final Logger LOGGER = Logger.getLogger(InQueueConsumer.class);
    
    private final BlockingQueue<Packet> outQueue;
    private final Properties conf;
    private final BlockingQueue<Packet> inQueue;
    private final ChannelManagerFactory channelManagerFactory;
    
    private final PacketReceiver packetReceiver;

    public InQueueConsumer(BlockingQueue<Packet> outQueue, 
            Properties conf, BlockingQueue<Packet> inQueue, ChannelManagerFactory channelManagerFactory, PacketReceiver packetReceiver) {
        super(inQueue);
        this.outQueue = outQueue;
        this.conf = conf;
        this.inQueue = inQueue;
        this.channelManagerFactory = channelManagerFactory;
        this.packetReceiver = packetReceiver;
    }

    @Override
    protected void consume(Packet p) {
    	ChannelManager channelManager = null;
        try {
            Long start = System.currentTimeMillis();

            String xml = p.toXML();
            LOGGER.debug("Received payload: '" + xml + "'.");
            channelManager = channelManagerFactory.create();
            if (p instanceof IQ) {
            	new IQProcessor(outQueue, conf, channelManager).process((IQ) p);
            } else if (p instanceof Message) {
            	new MessageProcessor(outQueue, inQueue, conf, channelManager).process((Message) p);
            } else {
                LOGGER.info("Not handling following stanzas yet: '" + xml + "'.");
            }

           	packetReceiver.packetReceived(p);
            
            LOGGER.debug("Payload handled in '"
                    + Long.toString((System.currentTimeMillis() - start))
                    + "' milliseconds.");

        } catch (Exception e) {
            LOGGER.debug("Exception: " + e.getMessage(), e);
        } finally {
        	try {
				channelManager.close();
			} catch (NodeStoreException e) {
				LOGGER.error("Error encountered while closing channel manager", e);
			}
        }
    }
}
