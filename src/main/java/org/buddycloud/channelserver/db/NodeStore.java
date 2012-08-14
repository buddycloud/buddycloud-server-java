package org.buddycloud.channelserver.db;

import java.util.Map;

import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.Node;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

/**
 * NodeStore is a interface for classes which have the ability to store and retrieve nodes
 * and user affiliations and subscriptions to those nodes.
 */
public interface NodeStore {
	/**
	 * Create a new node.
	 * @param owner the node owner.
	 * @param nodeID the node id.
	 * @param nodeConf a configuration map for the node.
	 */
	void createNode(JID owner, String nodeID, Map<String,String> nodeConf);
	
	Node fetchNode(String nodeID);
	
	boolean nodeExists(String nodeID);
	
	void setUserAffiliation(String nodeID, JID user, Affiliations affiliation);
	
	void setUserSubscription(String nodeID, JID user, Subscriptions subscription, String subscriptionId);
	
}
