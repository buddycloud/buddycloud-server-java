package org.buddycloud.channelserver.queue;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.IQProcessor;
import org.buddycloud.channelserver.packetprocessor.message.MessageProcessor;
import org.buddycloud.channelserver.packetprocessor.presence.PresenceProcessor;
import org.buddycloud.channelserver.utils.users.OnlineResourceManager;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.Presence;

public class InQueueConsumer extends QueueConsumer {

	private static final Logger logger = Logger
			.getLogger(InQueueConsumer.class);

	private final BlockingQueue<Packet> outQueue;
	private final Configuration conf;
	private final ChannelManagerFactory channelManagerFactory;
	private final FederatedQueueManager federatedQueueManager;

	private OnlineResourceManager onlineUsers;

	public InQueueConsumer(BlockingQueue<Packet> outQueue, Configuration conf,
			BlockingQueue<Packet> inQueue,
			ChannelManagerFactory channelManagerFactory,
			FederatedQueueManager federatedQueueManager, OnlineResourceManager onlineUsers) {
		super(inQueue);
		this.outQueue = outQueue;
		this.conf = conf;
		this.channelManagerFactory = channelManagerFactory;
		this.federatedQueueManager = federatedQueueManager;
		this.onlineUsers = onlineUsers;
	}

	@Override
	protected void consume(Packet p) {
		ChannelManager channelManager = null;
		try {
			Long start = System.currentTimeMillis();

			String xml = p.toXML();
			logger.debug("Received payload: '" + xml + "'.");
			channelManager = channelManagerFactory.create();
			if (p instanceof IQ) {
				new IQProcessor(outQueue, conf, channelManager,
						federatedQueueManager).process((IQ) p);
			} else if (p instanceof Message) {
				new MessageProcessor(outQueue, conf, channelManager)
						.process((Message) p);
			} else if (p instanceof Presence) {
				new PresenceProcessor(conf, onlineUsers).process((Presence) p);
			} else {
				logger.info("Not handling following stanzas yet: '" + xml
						+ "'.");
			}

			logger.debug("Payload handled in '"
					+ Long.toString((System.currentTimeMillis() - start))
					+ "' milliseconds.");

		} catch (Exception e) {
			logger.error("Exception: " + e.getMessage(), e);
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
