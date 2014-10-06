package org.buddycloud.channelserver.channel;

import java.util.List;

import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.xmpp.packet.JID;

public interface ChannelManager extends NodeStore {

	/**
	 * Creates a channel.
	 * 
	 * @param channelJID
	 *            the JID of the channel.
	 * @throws NodeStoreException
	 */
	void createPersonalChannel(JID ownerJID) throws NodeStoreException;
	
	/**
	 * Deletes all data from remote nodes
	 * 
	 * @throws NodeStoreException
	 */
	void deleteRemoteData() throws NodeStoreException;

	/**
	 * Gets the default affiliation for a node
	 * 
	 * @return
	 * 
	 * @throws NodeStoreException
	 */
	Affiliations getDefaultNodeAffiliation(String nodeId)
			throws NodeStoreException;

	/**
	 * Searches for the provided content or author, or both, across nodes
	 * the searcher has access to
	 * 
	 * @param searcher
	 * @param content
	 * @param author
	 * @param page
	 * @param rpp
	 * @return
	 */
	CloseableIterator<NodeItem> performSearch(JID searcher, List content, JID author, int page,
			int rpp) throws NodeStoreException;

}