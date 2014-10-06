package org.buddycloud.channelserver.packetHandler.iq.namespace.discoinfo;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo.DiscoResult;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class DiscoResultTest extends IQTestHandler {
    private IQ result;
    private DiscoResult discoResult;
    private ChannelManager channelManager;
    private FederatedQueueManager federatedQueueManager;

    @Before
    public void setUp() throws Exception {

        channelManager = Mockito.mock(ChannelManager.class);
        federatedQueueManager = Mockito.mock(FederatedQueueManager.class);

        discoResult = new DiscoResult(channelManager, federatedQueueManager);

        result = readStanzaAsIq("/iq/discoInfo/node-reply.stanza");
    }

    @Test
    public void testPassingThroughServerDiscoInfoResultStanzaHandsOffToDoServerDiscovery() throws Exception {
        result = toIq(readStanzaAsString("/iq/discoInfo/node-reply.stanza").replace("node=\"/user/romeo@shakespeare.lit/posts\"", ""));
        Mockito.when(federatedQueueManager.isFederatedDiscoInfoRequest(Mockito.anyString())).thenReturn(true);

        discoResult.process(result);

        Mockito.verify(federatedQueueManager, Mockito.times(1)).processDiscoInfoResponse(Mockito.any(JID.class), Mockito.anyString(), Mockito.anyList());
    }
}
