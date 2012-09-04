package org.buddycloud.channelserver.channel;

import java.util.Collection;
import java.util.Map;

import org.buddycloud.channelserver.channel.subscription.ChannelSubscription;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.xmpp.packet.JID;

public interface ChannelManager {
	
	/**
	 * Creates a channel.
	 * @param channelJID the JID of the channel.
	 * @param configuration the initial configuration of the channel. Missing metadata keys will be set to a default value.
	 */
	void createPersonalChannel(JID ownerJID, Map<String,String> configuration);

	/**
	 * Creates a channel.
	 * @param channelJID the JID of the channel.
	 * @param ownerJID the JID of the channel's creator/owner.
	 * @param configuration the initial configuration of the channel. Missing metadata keys will be set to a default value.
	 */
	void createTopicChannel(JID channelJID, JID ownerJID, Map<String,String> configuration);

	/**
	 * Edits an existing channel's configuration.
	 * @param channelJID the JID of the channel.
	 * @param configuration the initial configuration of the channel. Missing metadata keys will be left unchanged.
	 */
	void updateChannelConfiguration(JID channelJID, Map<String,String> configuration);
	
	/**
	 * Determines whether an entry for a channel with the given JID exists in the store.
	 * @param channelJID the JID of the channel.
	 * @return <code>true</code> if the channel exists, <code>false</code> otherwise.
	 */
	boolean channelExists(JID channelJID);
	
	/**
	 * Adds (or updates if already subscribed) a user's subscription with a channel.
	 * @param subscription the subscription to save
	 */
	void saveChannelSubscription(ChannelSubscription subscription);
	
	/**
	 * Returns the user's subscription type to the channel
	 * @param user the user's JID.
	 * @param channel the channel's JID.
	 * @return
	 */
	ChannelSubscription getUserChannelSubscription(JID user, JID channel);
	
	/**
	 * Returns all the channel subscriptions which the user has.
	 * @param user the user's JID.
	 * @return the collection of subscrptions.
	 */
	Collection<ChannelSubscription> getUserSubscriptions(JID user);
	
	/**
	 * Adds (or updates if already subscribed) a user's affiliation with a channel.
	 * @param user
	 * @param channelJID
	 * @param affiliation
	 */
	void setChannelAffiliation(JID user, JID channelJID, Affiliations affiliation);

	/**
	 * Posts an entry 
	 * @param user
	 * @param channelJID
	 * @param entry
	 */
	void postEntryToChannel(JID user, JID channelJID, String entry);
}
