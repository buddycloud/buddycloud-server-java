package org.buddycloud.channelserver.pubsub.model.impl;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeThread;
import org.xmpp.resultsetmanagement.ResultSet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class NodeThreadImpl implements NodeThread {

	private final String threadId;
	private final Date threadUpdated;
	private final List<NodeItem> nodeItems = new LinkedList<NodeItem>();

	public NodeThreadImpl(String threadId, Date threadUpdated) {
		this.threadId = threadId;
		this.threadUpdated = threadUpdated;
	}
	
	@Override
	public String getUID() {
		return threadId;
	}

	@Override
	public String getId() {
		return threadId;
	}

	@Override
	public Date getUpdated() {
		return threadUpdated;
	}

	public void addItem(NodeItem item) {
		nodeItems.add(item);
	}
	
	@Override
	public ResultSet<NodeItem> getItems() {
		return new ResultSetImpl<NodeItem>(nodeItems);
	}

}
