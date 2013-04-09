package org.buddycloud.channelserver.queue;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.IQProcessor;
import org.buddycloud.channelserver.packetprocessor.message.MessageProcessor;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class FederatedResponseQueueConsumer extends QueueConsumer {

	private static final Logger logger = Logger
			.getLogger(FederatedResponseQueueConsumer.class);

	private final BlockingQueue<Packet> federatedResponseQueue;
	private final Properties conf;
	private final ChannelManagerFactory channelManagerFactory;
	
	private boolean active = false;

	public FederatedResponseQueueConsumer(BlockingQueue<Packet> federatedResponseQueue, Properties conf,
			ChannelManagerFactory channelManagerFactory) {
		super(federatedResponseQueue);
		this.federatedResponseQueue = federatedResponseQueue;
		this.conf = conf;
		this.channelManagerFactory = channelManagerFactory;
		
		if (1 == Integer.parseInt(this.conf.getProperty("cache", "0"))) {
			this.active = true;
		}
	}

	@Override
	protected void consume(Packet p) {
		ChannelManager channelManager = null;
		
		if (false == active) return;
		
		try { 
			Long start = System.currentTimeMillis();

			String xml = p.toXML();
			logger.debug("Received payload: '" + xml + "'.");
			channelManager = channelManagerFactory.create();
			if (p instanceof IQ) {
			//	new IQProcessor(federatedResponseQueue, conf, channelManager).process((IQ) p);
			} else if (p instanceof Message) {
				//new MessageProcessor(federatedResponseQueue, conf, channelManager)
				//		.process((Message) p);
			} else {
				logger.info("Not handling following stanzas yet: '" + xml
						+ "'.");
			}

			logger.debug("Payload handled in '"
					+ Long.toString((System.currentTimeMillis() - start))
					+ "' milliseconds.");

		} catch (Exception e) {
			logger.debug("Exception: " + e.getMessage(), e);
		} finally {
			try {
				channelManager.close();
			} catch (NodeStoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
}
