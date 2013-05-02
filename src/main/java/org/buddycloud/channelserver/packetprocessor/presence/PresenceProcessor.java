package org.buddycloud.channelserver.packetprocessor.presence;

import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.utils.users.OnlineResourceManager;
import org.xmpp.packet.Presence;

public class PresenceProcessor implements PacketProcessor<Presence> {

	private OnlineResourceManager onlineUsers;
	private Properties configuration;
	
	private Logger logger = Logger.getLogger(PresenceProcessor.class);

	public PresenceProcessor(Properties conf, OnlineResourceManager onlineUsers) {
		this.configuration = conf;
		this.onlineUsers   = onlineUsers;
	}
	
	@Override
	public void process(Presence packet) throws Exception {
        logger.info("Recevied presence packet: " + packet.toXML());
	}
}