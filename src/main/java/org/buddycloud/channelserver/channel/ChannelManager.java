package org.buddycloud.channelserver.channel;

import java.util.List;
import java.util.Set;

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
	 * Determines whether the node id given refers to a local node.
	 * 
	 * @param nodeId
	 *            the node id
	 * @return <code>true</code> if the node appears to be local,
	 *         <code>false</code> otherwise.
	 * @throws NodeStoreException
	 */
	boolean isLocalNode(String nodeId) throws NodeStoreException;

	/**
	 * Determines whether the jid refers to a local user.
	 * 
	 * @param jid
	 *            the user's jid
	 * @return <code>true</code> if the jid appears to be local,
	 *         <code>false</code> otherwise.
	 * @throws NodeStoreException
	 */
	boolean isLocalJID(JID jid) throws NodeStoreException;
	
	/**
	 * Determines whether the domain is served by this server.
	 * 
	 * @param domain
	 *            the domain name
	 * @return <code>true</code> if the domain appears to be local,
	 *         <code>false</code> otherwise.
	 * @throws NodeStoreException
	 */
	boolean isLocalDomain(String domain) throws NodeStoreException;

	
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

	/**
	 * @return
	 */
	Set<String> getLocalDomains();

	boolean isLocalNode(String nodeId, Set<String> localDomains);

}