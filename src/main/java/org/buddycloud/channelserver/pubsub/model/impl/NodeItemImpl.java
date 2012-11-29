package org.buddycloud.channelserver.pubsub.model.impl;

import java.util.Date;

import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.xmpp.resultsetmanagement.Result;

public class NodeItemImpl implements NodeItem {

	private final String nodeId;
	private final String id;
	private final String payload;
	private final Date updated;
	
	public NodeItemImpl(final String nodeId, final String id, final Date updated, final String payload) {
		this.nodeId = nodeId;
		this.id = id;
		this.updated = updated;
		this.payload = payload;
	}

	@Override
	public String getNodeId() {
		return nodeId;
	}
	
	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getPayload() {
		return payload;
	}

	@Override
	public Date getUpdated() {
		return updated;
	}

	@Override
	public String getUID() {
		return id;
	}
}
