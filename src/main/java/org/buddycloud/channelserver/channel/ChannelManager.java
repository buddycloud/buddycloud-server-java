package org.buddycloud.channelserver.channel;

import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.xmpp.packet.JID;

public interface ChannelManager extends NodeStore {
	
	/**
	 * Creates a channel.
	 * @param channelJID the JID of the channel.
	 * @throws NodeStoreException 
	 */
	void createPersonalChannel(JID ownerJID) throws NodeStoreException;
	
	/**
	 * Determines whether the node id given refers to a local node.
	 * @param nodeId the node id
	 * @return <code>true</code> if the node appears to be local, <code>false</code> otherwise.
	 * @throws NodeStoreException 
	 */
	boolean isLocalNode(String nodeId) throws NodeStoreException;
	
	/**
	 * Determines whether the jid refers to a local user.
	 * @param jid the user's jid
	 * @return <code>true</code> if the jid appears to be local, <code>false</code> otherwise.
	 * @throws NodeStoreException 
	 */
	boolean isLocalJID(JID jid) throws NodeStoreException;

	/**
	 * Deletes all data from remote nodes
	 * 
	 * @throws NodeStoreException
	 */
	void deleteRemoteData() throws NodeStoreException; 
}
