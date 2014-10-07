package org.buddycloud.channelserver.sync;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelManagerFactory;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.xmpp.packet.Packet;

public class ServerSync {

    private ChannelManager channelManager;
    private Configuration configuration;

    private Logger logger = Logger.getLogger(ServerSync.class);

    public ServerSync(ChannelManagerFactory channelManagerFactory, BlockingQueue<Packet> outQueue, BlockingQueue<Packet> inQueue,
            Configuration configuration) {
        this.channelManager = channelManagerFactory.create();
        this.configuration = configuration;
    }

    public void start() {
        try {
            deleteFederatedChannelCache();
            channelManager.close();
        } catch (NodeStoreException e) {
            logger.error(e);
        }
    }

    private void deleteFederatedChannelCache() throws NodeStoreException {
        Boolean purge = Boolean.valueOf(configuration.getProperty(Configuration.PURGE_REMOTE_ON_START, "false"));
        if (false == purge) {
            return;
        }
        channelManager.deleteRemoteData();
    }
}
