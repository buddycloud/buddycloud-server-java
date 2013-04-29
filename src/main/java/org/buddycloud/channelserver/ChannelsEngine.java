package org.buddycloud.channelserver;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.channel.ChannelManagerFactoryImpl;
import org.buddycloud.channelserver.db.DefaultNodeStoreFactoryImpl;
import org.buddycloud.channelserver.db.NodeStoreFactory;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.buddycloud.channelserver.queue.InQueueConsumer;
import org.buddycloud.channelserver.queue.OutQueueConsumer;
import org.buddycloud.channelserver.sync.ServerSync;
import org.xmpp.component.Component;
import org.xmpp.component.ComponentException;
import org.xmpp.component.ComponentManager;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class ChannelsEngine implements Component {

	private static final Logger LOGGER = Logger.getLogger(ChannelsEngine.class);

	private JID jid = null;
	private ComponentManager manager = null;

	private BlockingQueue<Packet> outQueue = new LinkedBlockingQueue<Packet>();
	private BlockingQueue<Packet> inQueue = new LinkedBlockingQueue<Packet>();
	
	private ChannelManagerFactory channelManagerFactory;
	private FederatedQueueManager federatedQueueManager;
	
	private ServerSync serverSync;
	
	private Properties conf;

	public ChannelsEngine(Properties conf) {
		this.conf = conf;
	}

	@Override
	public String getDescription() {
		return "buddycloud channel server (Java implementation)";
	}

	@Override
	public String getName() {
		return "buddycloud-channel-server";
	}

	@Override
	public void initialize(JID jid, ComponentManager manager)
			throws ComponentException {

		this.jid = jid;
		this.manager = manager;

		setupManagers();
		startQueueConsumers();
        serverSync();
		LOGGER.info("XMPP Component started. We are '" + jid.toString()
				+ "' and ready to accept packages.");
	}

	private void serverSync() {
		serverSync = new ServerSync(channelManagerFactory, inQueue, outQueue);
		serverSync.start();
	}

	private void startQueueConsumers() {
		OutQueueConsumer outQueueConsumer = new OutQueueConsumer(this,
				outQueue, federatedQueueManager,
				conf.getProperty("server.domain"));

		InQueueConsumer inQueueConsumer = new InQueueConsumer(outQueue, conf,
				inQueue, channelManagerFactory, federatedQueueManager);
		
		outQueueConsumer.start();
		inQueueConsumer.start();
	}

	private void setupManagers() throws ComponentException {
		NodeStoreFactory nodeStoreFactory;
		try {
			nodeStoreFactory = new DefaultNodeStoreFactoryImpl(conf);
		} catch (NodeStoreException e) {
			throw new ComponentException(e);
		}
		
		channelManagerFactory = new ChannelManagerFactoryImpl(
				conf, nodeStoreFactory);
		federatedQueueManager = new FederatedQueueManager(
				this, conf.getProperty("server.domain.channels"));
	}

	@Override
	public void processPacket(Packet p) {
		try {
			this.inQueue.put(p);
		} catch (InterruptedException e) {
			LOGGER.error(p);
		}
	}

	public void sendPacket(Packet p) throws ComponentException {
		manager.sendPacket(this, p);
	}

	@Override
	public void shutdown() {
		// TODO Auto-generated method stub
	}

	@Override
	public void start() {
		/**
		 * Notification message indicating that the component will start
		 * receiving incoming packets. At this time the component may finish
		 * pending initialization issues that require information obtained from
		 * the server.
		 * 
		 * It is likely that most of the component will leave this method empty.
		 */
	}

	public JID getJID() {
		return this.jid;
	}
}