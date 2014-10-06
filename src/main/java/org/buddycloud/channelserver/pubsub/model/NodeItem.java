package org.buddycloud.channelserver.pubsub.model;

import java.util.Date;

import org.xmpp.resultsetmanagement.Result;

public interface NodeItem extends Result {
    /**
     * Retrieves the node id.
     * 
     * @return
     */
    String getNodeId();

    /**
     * Retrieves the item id.
     * 
     * @return
     */
    String getId();

    /**
     * Retrieves the payload.
     * 
     * @return
     */
    String getPayload();

    /**
     * Retrieves the datestamp when the record was published/updated
     */
    Date getUpdated();

    /**
     * Retrieves the item ID to which this item is a reply Not a reply == null
     */
    String getInReplyTo();

    /**
     * Retrieves the datestamp when the record was first published
     */
    Date getCreated();
}
