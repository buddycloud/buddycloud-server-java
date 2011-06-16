package org.buddycloud.channels;

import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.InQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.xmpp.component.Component;
import org.xmpp.component.ComponentException;
import org.xmpp.component.ComponentManager;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class ChannelsEngine implements Component {

	private static Logger LOGGER = Logger.getLogger(ChannelsEngine.class);
	
	private JID jid = null;
	private ComponentManager manager = null;
	
	private OutQueue outQueue = null;
	private InQueue inQueue = null;
	private ErrorQueue errorQueue = null;

	private Properties conf;
	
	public ChannelsEngine(Properties conf) {
		this.conf = conf;
	}
	
	@Override
	public String getDescription() {
		return "Description (TODO)";
	}

	@Override
	public String getName() {
		return "Description (TODO)";
	}

	@Override
	public void initialize(JID jid, ComponentManager manager) throws ComponentException {
		
		this.jid = jid;
		this.manager = manager;
		
		this.outQueue = new OutQueue(manager, this, true);
		this.errorQueue = new ErrorQueue(this.outQueue);
		this.inQueue = new InQueue(this.outQueue, this.errorQueue, this.conf);
		
		LOGGER.info("XMPP Component started. We are '" + jid.toBareJID() + "' and ready to accept packages.");
	}

	public OutQueue getOutQueue() {
		return this.outQueue;
	}
	
	@Override
	public void processPacket(Packet p) {
		this.inQueue.put(p);
	}

	@Override
	public void shutdown() {
		// TODO Auto-generated method stub
	}

	@Override
	public void start() {
		/**
		 * Notification message indicating that the component will start receiving 
		 * incoming packets. At this time the component may finish pending initialization 
		 * issues that require information obtained from the server.
         *
		 * It is likely that most of the component will leave this method empty. 
		 */
	}
	
	public JID getJID() {
		return this.jid;
	}

}
