package org.buddycloud.channelserver.pubsub.model;

import java.util.Date;

import org.xmpp.resultsetmanagement.Result;

public abstract class NodeItem implements Result {
	/**
	 * Retrieves the node id.
	 * @return
	 */
	public abstract String getNodeId();

	/**
	 * Retrieves the item id.
	 * @return
	 */
	public abstract String getId();
	
	/**
	 * Retrieves the payload.
	 * @return
	 */
	public abstract String getPayload();
	
	/**
	 * Retrieves the datestamp when the record was published/updated
	 */
	public abstract Date getUpdated();
}
