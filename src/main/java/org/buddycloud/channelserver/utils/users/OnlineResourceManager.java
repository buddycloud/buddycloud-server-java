package org.buddycloud.channelserver.utils.users;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.xmpp.packet.JID;

public class OnlineResourceManager {

	public static final String UNAVAILABLE = "unavailable";
	
	private String domain;
	private HashMap<String, ArrayList<JID>> users = new HashMap<String, ArrayList<JID>>();

	private Logger logger = Logger.getLogger(OnlineResourceManager.class);

	public OnlineResourceManager(Properties conf) {
		this.domain = conf.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN);

		if (null == this.domain)
			throw new NullPointerException("Missing server domain configuration");
	}

	public ArrayList<JID> getResources(JID jid) {
		if ((null != jid.getResource())
		    || ((null == jid.getResource()) && (null == jid.getNode()))) {
			ArrayList<JID> user = new ArrayList<JID>();
			user.add(jid);
			return user;
		}
		
        if (false == users.containsKey(jid.toBareJID()))
		    return new ArrayList<JID>();
        return users.get(jid.toBareJID());
	}

	public void updateStatus(JID jid, String status) {
		if (false == jid.getDomain().equals(domain)) return;
		if (status.equals(UNAVAILABLE)) {
			logger.info("User going offline: " + jid.toFullJID());
			if (true == users.containsKey(jid.toBareJID())) {
				ArrayList<JID> user = users.get(jid.toBareJID());
				user.remove(jid);
			}
			return;
		}
		if (false == users.containsKey(jid.toBareJID())) {
			users.put(jid.toBareJID(), new ArrayList<JID>());
		}
		ArrayList<JID> entry = users.get(jid.toBareJID());
		if (!entry.contains(jid)) entry.add(jid);
		logger.info("User now online: " + jid.toFullJID());
	}
}