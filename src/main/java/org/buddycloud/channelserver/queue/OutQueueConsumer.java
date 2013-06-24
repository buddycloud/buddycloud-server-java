package org.buddycloud.channelserver.queue;

import java.util.ArrayList;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;
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
	private String server;
	private OnlineResourceManager onlineUsers;

	public OutQueueConsumer(ChannelsEngine component,
			BlockingQueue<Packet> outQueue,
			FederatedQueueManager federatedQueue, String server,
			OnlineResourceManager onlineUsers) {
		super(outQueue);
		this.component = component;
		this.federatedQueue = federatedQueue;
		this.server = server;
		this.onlineUsers = onlineUsers;
	}

	@Override
	protected void consume(Packet p) {

		try {
			if ((-1 == p.getTo().toString().indexOf("@"))
					&& (p.getTo().toBareJID().indexOf(server) == -1)) {
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
			// Get a list of 'online' resources for this JID
			ArrayList<JID> resources = onlineUsers.getResources(p.getTo());
			logger.debug("There are " + resources.size() + " online resources for " + p.getTo());
			for (JID resource : resources) {
		        p.setTo(resource);
			    component.sendPacket(p.createCopy());
			    logger.debug("OUT -> " + p.toXML());
			}
		} catch (ComponentException e) {
			logger.error("Sending packet caused error: " + p.toXML());
			logger.error(e);
			e.printStackTrace();
		}
	}
}