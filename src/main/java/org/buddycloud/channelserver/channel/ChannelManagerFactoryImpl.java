package org.buddycloud.channelserver.channel;

import java.util.Properties;

import org.buddycloud.channelserver.db.NodeStoreFactory;

public class ChannelManagerFactoryImpl implements ChannelManagerFactory {

    private final Properties configuration;
    private final NodeStoreFactory nodeStoreFactory;

    public ChannelManagerFactoryImpl(final Properties configuration, final NodeStoreFactory nodeStoreFactory) {
        this.configuration = configuration;
        this.nodeStoreFactory = nodeStoreFactory;
    }

    @Override
    public ChannelManager create() {
        return new ChannelManagerImpl(nodeStoreFactory.create(), configuration);
    }

}
