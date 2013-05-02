package org.buddycloud.channelserver.packetprocessor.presence;

import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.utils.users.OnlineResourceManager;
import org.dom4j.Element;
import org.xmpp.packet.JID;
import org.xmpp.packet.Presence;

public class PresenceProcessor implements PacketProcessor<Presence> {

	private OnlineResourceManager onlineUsers;
	
	private Logger logger = Logger.getLogger(PresenceProcessor.class);
	private String domain;

	public PresenceProcessor(Properties configuration, OnlineResourceManager onlineUsers) {
		this.domain = configuration.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN);
		this.onlineUsers = onlineUsers;
		
		if (null == this.domain) 
			throw new NullPointerException("Missing server domain configuration");
	}
	
	@Override
	public void process(Presence packet) throws Exception {
		JID from = packet.getFrom();
        if ((null == from) || (false == from.getDomain().equals(domain))) return;
        Element show = packet.getElement().element("show");
        if (null == show) return;
        onlineUsers.updateStatus(packet.getFrom(), show.getText());
	}
}