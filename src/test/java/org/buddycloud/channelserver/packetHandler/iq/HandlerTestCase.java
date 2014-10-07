package org.buddycloud.channelserver.packetHandler.iq;

import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.TestHelper;
import org.junit.Before;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.xmpp.packet.Packet;

public class HandlerTestCase {
    protected TestHelper helper;

    @Mock
    protected Configuration config;

    protected LinkedBlockingQueue<Packet> outQueue;
    protected LinkedBlockingQueue<Packet> inQueue;

    /**
     * A channel manager which can be used to initialise nodes, etc. for tests.
     */
    protected ChannelManager channelManager;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        helper = new TestHelper();

        this.outQueue = helper.getOutQueue();
        this.inQueue = helper.getInQueue();

        channelManager = helper.getChannelManagerFactory().create();
    }
}
