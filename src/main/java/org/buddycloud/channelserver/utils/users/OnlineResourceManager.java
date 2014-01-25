package org.buddycloud.channelserver.utils.users;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.channel.LocalDomainChecker;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.Presence;
import org.xmpp.packet.Presence.Type;
import org.xmpp.resultsetmanagement.ResultSet;

public class OnlineResourceManager {

	public static final String UNAVAILABLE = "unavailable";
	private static final Logger LOGGER = Logger.getLogger(OnlineResourceManager.class);
	
	private String domain;
	private HashMap<String, ArrayList<JID>> users = new HashMap<String, ArrayList<JID>>();
	private Properties conf;

	public OnlineResourceManager(Properties conf) {
		this.domain = conf.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN);
		if (this.domain == null) {
			throw new NullPointerException("Missing server domain configuration");
		}
		this.conf = conf;
	}

	public void subscribeToNodeListeners(ChannelManagerFactory channelManagerFactory, 
			BlockingQueue<Packet> outQueue) {
		ChannelManager channelManager = channelManagerFactory.create();
		try {
			ResultSet<NodeSubscription> subscriptions = channelManager.getNodeSubscriptionListeners();
			for (NodeSubscription subscription : subscriptions) {
				Presence presence = new Presence(Type.subscribe);
				presence.setTo(subscription.getListener().toBareJID());
				outQueue.put(presence);
			}
		} catch (Exception e) {
			LOGGER.warn("Error while sending presence to node listeners", e);
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
		if (!jid.getDomain().equals(domain) && 
				!LocalDomainChecker.isLocal(jid.getDomain(), conf)) {
			return;
		}
		if (type != null && type.equals(UNAVAILABLE)) {
			LOGGER.info("User going offline: " + jid.toString());
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