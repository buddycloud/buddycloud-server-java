package org.buddycloud.channelserver.pubsub.model.impl;

import java.util.Date;

import org.buddycloud.channelserver.pubsub.model.NodeItem;

public class NodeItemImpl implements NodeItem {

	private final String nodeId;
	private final String id;
	private final String payload;
	private final Date updated;
	private final Date created;
	private final String inReplyTo;

	public NodeItemImpl(final String nodeId, final String id,
			final Date updated, final String payload) {
		this(nodeId, id, updated, payload, null, null);
	}

	public NodeItemImpl(final String nodeId, final String id,
			final Date updated, final String payload, final String inReplyTo) {
		this(nodeId, id, updated, payload, inReplyTo, null);
	}

	public NodeItemImpl(final String nodeId, final String id,
			final Date updated, final String payload, final String inReplyTo,
			final Date created) {
		this.nodeId = nodeId;
		this.id = id;
		this.updated = updated;
		this.created = created;
		this.payload = payload;
		this.inReplyTo = inReplyTo;
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

	@Override
	public String getInReplyTo() {
		return inReplyTo;
	}

	@Override
	public String toString() {
		return "NodeItemImpl [nodeId=" + nodeId + ", id=" + id + ", payload="
				+ payload + ", updated=" + updated + ", inReplyTo=" + inReplyTo
				+ ", created=" + created + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((nodeId == null) ? 0 : nodeId.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof NodeItem))
			return false;
		NodeItem other = (NodeItem) obj;
		if (id == null) {
			if (other.getId() != null)
				return false;
		} else if (!id.equals(other.getId()))
			return false;
		if (nodeId == null) {
			if (other.getNodeId() != null)
				return false;
		} else if (!nodeId.equals(other.getNodeId()))
			return false;
		return true;
	}

	@Override
	public Date getCreated() {
		return created;
	}
}
