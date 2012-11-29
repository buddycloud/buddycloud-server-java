package org.buddycloud.channelserver.pubsub.model;

import java.util.Date;

import org.xmpp.resultsetmanagement.Result;

public interface NodeItem extends Result {
	/**
	 * Retrieves the node id.
	 * @return
	 */
	String getNodeId();

	/**
	 * Retrieves the item id.
	 * @return
	 */
	String getId();
	
	/**
	 * Retrieves the payload.
	 * @return
	 */
	String getPayload();
	
	/**
	 * Retrieves the datestamp when the record was published/updated
	 */
	Date getUpdated();
}
