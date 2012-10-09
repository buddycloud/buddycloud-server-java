package org.buddycloud.channelserver.pubsub.model;

import java.util.Date;

public interface NodeItem {
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
