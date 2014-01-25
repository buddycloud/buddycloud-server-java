package org.buddycloud.channelserver.packetprocessor.presence;

import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.LocalDomainChecker;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.utils.users.OnlineResourceManager;
import org.xmpp.packet.JID;
import org.xmpp.packet.Presence;

public class PresenceProcessor implements PacketProcessor<Presence> {
	
	private static final Logger LOGGER = Logger.getLogger(PresenceProcessor.class);

	private final Properties configuration;
	private OnlineResourceManager onlineUsers;
	private String domain;

	public PresenceProcessor(Properties configuration, OnlineResourceManager onlineUsers) {
		this.domain = configuration.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN);
		this.configuration = configuration;
		this.onlineUsers = onlineUsers;
		
		if (null == this.domain) 
			throw new NullPointerException("Missing server domain configuration");
	}
	
	@Override
	public void process(Presence packet) throws Exception {
		JID from = packet.getFrom();
        if (from == null || (!from.getDomain().equals(domain) && 
        		!LocalDomainChecker.isLocal(from.getDomain(), configuration))) {
        	return;
        }
        
        LOGGER.debug("Processing presence from " + from.toString());
        
        String type = packet.getElement().attributeValue("type");
        onlineUsers.updateStatus(packet.getFrom(), type);
	}
}