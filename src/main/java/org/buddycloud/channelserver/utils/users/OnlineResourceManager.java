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
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.Presence;
import org.xmpp.packet.Presence.Type;
import org.xmpp.resultsetmanagement.ResultSet;

public class OnlineResourceManager {

	public static final String UNAVAILABLE = "unavailable";
	private static final Logger LOGGER = Logger
			.getLogger(OnlineResourceManager.class);

	private HashMap<String, ArrayList<JID>> users = new HashMap<String, ArrayList<JID>>();
	private Properties configuration;
	private ChannelManager channelManager;
	private Boolean useDatabaseStorage = false;

	public OnlineResourceManager(Properties configuration,
			ChannelManager channelManager) {
		if (configuration
				.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN) == null
				&& configuration
						.getProperty(Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER) == null) {
			throw new NullPointerException(
					"Missing server domain configuration");

		}
		this.configuration = configuration;
		this.channelManager = channelManager;
		this.useDatabaseStorage = Boolean.valueOf(configuration.getProperty(
				Configuration.PERSIST_PRESENCE_DATA, "false"));

	}

	public void subscribeToNodeListeners(BlockingQueue<Packet> outQueue) {
		try {
			ResultSet<NodeSubscription> subscriptions = channelManager
					.getNodeSubscriptionListeners();
			for (NodeSubscription subscription : subscriptions) {
				Presence presence = new Presence(Type.subscribe);
				presence.setTo(subscription.getListener().toBareJID());
				outQueue.put(presence);
			}
		} catch (Exception e) {
			LOGGER.warn("Error while sending presence to node listeners", e);
		}
	}

	public ArrayList<JID> getResources(JID jid) throws NodeStoreException {
		if ((jid.getResource() != null)
				|| ((jid.getResource() == null) && (jid.getNode() == null))) {
			ArrayList<JID> user = new ArrayList<JID>();
			user.add(jid);
			return user;
		}
		if (true == useDatabaseStorage) {
			ArrayList<JID> jids = channelManager.onlineJids(jid);
		}
		if (!users.containsKey(jid.toBareJID())) {
			return new ArrayList<JID>();
		}
		return users.get(jid.toBareJID());
	}

	public void updateStatus(JID jid, String type) throws NodeStoreException {
		if (!LocalDomainChecker.isLocal(jid.getDomain(), configuration)) {
			return;
		}
		
		ArrayList<JID> user = null;
		if (users.containsKey(jid.toBareJID())) {
			user = users.get(jid.toBareJID());
		}
		if ((type != null) && type.equals(UNAVAILABLE)) {
			LOGGER.info("User going offline: " + jid.toString());
			if (true == useDatabaseStorage) {
				channelManager.jidOffline(jid);
			} else {
				if (null != user) {
					user.remove(jid);
				}
			}
			return;
		}
		if (true == useDatabaseStorage) {
			channelManager.jidOnline(jid);
		} else {
			if (null == user) {
				users.put(jid.toBareJID(), new ArrayList<JID>());
			}
			ArrayList<JID> entry = users.get(jid.toBareJID());
			if (!entry.contains(jid)) {
				entry.add(jid);
			}
		}
		LOGGER.info("User now online: " + jid.toFullJID());
	}
}