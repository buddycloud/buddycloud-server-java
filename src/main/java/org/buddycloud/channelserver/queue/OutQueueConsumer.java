package org.buddycloud.channelserver.queue;

import java.util.ArrayList;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.utils.users.OnlineResourceManager;
import org.dom4j.Attribute;
import org.xmpp.component.ComponentException;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class OutQueueConsumer extends QueueConsumer {

	private static final Logger logger = Logger
			.getLogger(OutQueueConsumer.class);
	private final ChannelsEngine component;
	private FederatedQueueManager federatedQueue;
	private Configuration conf;
	private OnlineResourceManager onlineUsers;
	private BlockingQueue<Packet> inQueue;

	public OutQueueConsumer(ChannelsEngine component,
			BlockingQueue<Packet> outQueue,
			FederatedQueueManager federatedQueue, Configuration conf,
			OnlineResourceManager onlineUsers, BlockingQueue<Packet> inQueue) {
		super(outQueue);
		this.component = component;
		this.federatedQueue = federatedQueue;
		this.conf = conf;
		this.onlineUsers = onlineUsers;
		this.inQueue = inQueue;
	}

	@Override
	protected void consume(Packet p) {

		try {
			if (isRemoteServer(p.getTo())) {
				// i.e. a remote server
				if (null == p.getElement().attributeValue(
						"remote-server-discover")) {
					federatedQueue.process(p);
					return;
				}
				federatedQueue.addChannelMap(p.getTo());
			}

			// Clean federation marks from packet
			if (p.getElement().attribute("remote-server-discover") != null) {
				Attribute process = p.getElement().attribute(
						"remote-server-discover");
				p.getElement().remove(process);
			}

			// If it's just a request for the local server then just push it
			// straight back into the inqueue
			if (shouldRouteToLocalChannelServer(p.getTo())) {
				inQueue.put(p);
				return;
			}

			// Get a list of 'online' resources for this JID
			ArrayList<JID> resources = onlineUsers.getResources(p.getTo());
			logger.debug("There are " + resources.size()
					+ " online resources for " + p.getTo());
			for (JID resource : resources) {
				p.setTo(resource);
				logger.debug("OUT -> " + p.toXML());
				component.sendPacket(p.createCopy());
			}
		} catch (ComponentException e) {
			logger.error("Sending packet caused error: " + p.toXML(), e);
		} catch (InterruptedException e) {
			logger.error("Sending packet caused error: " + p.toXML(), e);
		}
	}

	/**
	 * Returns true if the JID points to a server (with no node) and if it is
	 * not one of the local domains.
	 * 
	 * @param jid
	 * @return
	 */
	private boolean isRemoteServer(JID jid) {
		if (jid.getNode() != null) {
			return false;
		}

		String domain = jid.getDomain();

		if (domain.equals(conf.getServerDomain())) {
			return false;
		}

		if (domain.equals(conf.getServerChannelsDomain())) {
			return false;
		}

		if (domain.equals(conf.getServerTopicsDomain())) {
			return false;
		}

		return true;
	}

	/**
	 * Returns <code>true</code> if the jid should route through to this
	 * component.
	 * 
	 * @param jid
	 * @return
	 */
	private boolean shouldRouteToLocalChannelServer(JID jid) {
		if (jid.getNode() != null) {
			return false;
		}

		String domain = jid.getDomain();

		if (domain.equals(conf.getServerChannelsDomain())) {
			return true;
		}

		if (domain.equals(conf.getServerTopicsDomain())) {
			return true;
		}

		return false;
	}
}