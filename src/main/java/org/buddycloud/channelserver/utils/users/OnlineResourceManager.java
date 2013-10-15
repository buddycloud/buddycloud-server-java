package org.buddycloud.channelserver.utils.users;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.xmpp.packet.JID;

public class OnlineResourceManager {

	public static final String UNAVAILABLE = "unavailable";
	private static final Logger LOGGER = Logger.getLogger(OnlineResourceManager.class);
	
	private String domain;
	private HashMap<String, ArrayList<JID>> users = new HashMap<String, ArrayList<JID>>();

	public OnlineResourceManager(Properties conf) {
		this.domain = conf.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN);
		if (this.domain == null) {
			throw new NullPointerException("Missing server domain configuration");
		}
	}

	public ArrayList<JID> getResources(JID jid) {
		if (jid.getResource() != null || (jid.getResource() == null && jid.getNode() == null)) {
			ArrayList<JID> user = new ArrayList<JID>();
			user.add(jid);
			return user;
		}
        if (!users.containsKey(jid.toBareJID())) {
        	return new ArrayList<JID>();
        }
        return users.get(jid.toBareJID());
	}

	public void updateStatus(JID jid, String type) {
		if (!jid.getDomain().equals(domain)) {
			return;
		}
		if (type != null && type.equals(UNAVAILABLE)) {
			LOGGER.info("User going offline: " + jid.toFullJID());
			if (users.containsKey(jid.toBareJID())) {
				ArrayList<JID> user = users.get(jid.toBareJID());
				user.remove(jid);
			}
			return;
		}
		if (!users.containsKey(jid.toBareJID())) {
			users.put(jid.toBareJID(), new ArrayList<JID>());
		}
		ArrayList<JID> entry = users.get(jid.toBareJID());
		if (!entry.contains(jid)) {
			entry.add(jid);
		}
		LOGGER.info("User now online: " + jid.toFullJID());
	}
}