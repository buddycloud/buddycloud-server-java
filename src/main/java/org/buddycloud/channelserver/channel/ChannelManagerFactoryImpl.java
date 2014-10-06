package org.buddycloud.channelserver.channel;

import org.buddycloud.channelserver.db.NodeStoreFactory;

public class ChannelManagerFactoryImpl implements ChannelManagerFactory {

	private final NodeStoreFactory nodeStoreFactory;
	
	public ChannelManagerFactoryImpl(final NodeStoreFactory nodeStoreFactory) {
		this.nodeStoreFactory = nodeStoreFactory;
	}
	
	@Override
	public ChannelManager create() {
		return new ChannelManagerImpl(nodeStoreFactory.create());
	}

}
