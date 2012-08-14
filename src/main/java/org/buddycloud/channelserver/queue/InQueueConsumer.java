package org.buddycloud.channelserver.queue;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.jedis.JedisMongoDataStore;
import org.buddycloud.channelserver.packetprocessor.iq.IQProcessor;
import org.buddycloud.channelserver.packetprocessor.message.MessageProcessor;

import org.apache.log4j.Logger;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class InQueueConsumer extends QueueConsumer {

    private static final Logger LOGGER = Logger.getLogger(InQueueConsumer.class);
    
    private JedisMongoDataStore dataStore;
    private MessageProcessor messageHandler;
    private IQProcessor iqHandler;

    public InQueueConsumer(BlockingQueue<Packet> outQueue, 
            Properties conf, BlockingQueue<Packet> inQueue) {
        super(inQueue);
        this.dataStore = new JedisMongoDataStore(conf);
        this.iqHandler = new IQProcessor(outQueue, conf, this.dataStore);
        this.messageHandler = new MessageProcessor(outQueue, inQueue, conf, dataStore);
    }

    @Override
    protected void consume(Packet p) {
        try {
            Long start = System.currentTimeMillis();

            String xml = p.toXML();
            LOGGER.debug("Received payload: '" + xml + "'.");

            if (p instanceof IQ) {
                this.iqHandler.process((IQ) p);
            } else if (p instanceof Message) {
                this.messageHandler.process((Message) p);
            } else {
                LOGGER.info("Not handling following stanzas yet: '" + xml + "'.");
            }

            LOGGER.debug("Payload handled in '"
                    + Long.toString((System.currentTimeMillis() - start))
                    + "' milliseconds.");

        } catch (Exception e) {
            LOGGER.debug("Exception: " + e.getMessage(), e);
        }
    }

}
