package org.buddycloud.channelserver.sync;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.queue.InQueueConsumer;
import org.buddycloud.channelserver.queue.OutQueueConsumer;
import org.xmpp.packet.Packet;

public class ServerSync {

	private ChannelManager channelManager;
	private BlockingQueue<Packet> inQueue;
	private BlockingQueue<Packet> outQueue;

	private Logger logger = Logger.getLogger(ServerSync.class);
	
	public ServerSync(ChannelManagerFactory channelManagerFactory,
			BlockingQueue<Packet> outQueue, BlockingQueue<Packet> inQueue) {
		this.channelManager = channelManagerFactory.create();
		this.outQueue = outQueue;
		this.inQueue = inQueue;
	}

	public void start() {
		try {
		    deleteFederatedChannelCache();
		    channelManager.close();
		} catch (NodeStoreException e) {
			logger.error(e);
		}
	}

	private void deleteFederatedChannelCache() throws NodeStoreException {
		channelManager.deleteRemoteData();
	}
}
