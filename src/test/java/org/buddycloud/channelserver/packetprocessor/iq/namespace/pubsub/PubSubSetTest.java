package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class PubSubSetTest extends IQTestHandler {

  private ChannelManager channelManager;
  private LinkedBlockingQueue<Packet> queue;
  private PubSubSet pubsubSet;

  @Before
  public void setUp() throws Exception {
    channelManager = Mockito.mock(ChannelManager.class);
    queue = new LinkedBlockingQueue<Packet>();
    pubsubSet = new PubSubSet(queue, channelManager);
    pubsubSet.purgeElementProcessors();
  }

  @Test
  public void returnsErrorForIllegalActor() throws Exception {
    IQ request = readStanzaAsIq("/iq/pubsub/bad-actor.stanza");

    pubsubSet.process(request);
    Assert.assertEquals(1, queue.size());
    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.error, response.getType());

    PacketError error = response.getError();
    Assert.assertNotNull(error);

    Assert.assertEquals(PacketError.Type.cancel, error.getType());
    Assert.assertNotNull(response.getElement().element("error").element(XMLConstants.POLICY_VIOLATION));
    Assert.assertEquals(XMLConstants.INVALID_NODE, error.getApplicationConditionName());
    Assert.assertEquals(Buddycloud.NS_ERROR, error.getApplicationConditionNamespaceURI());

  }

  @Test
  public void returnsErrorForSpoofActor() throws Exception {
    IQ request = readStanzaAsIq("/iq/pubsub/spoof-actor.stanza");

    pubsubSet.process(request);
    Assert.assertEquals(1, queue.size());
    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.error, response.getType());

    PacketError error = response.getError();
    Assert.assertNotNull(error);

    Assert.assertEquals(PacketError.Type.cancel, error.getType());
    Assert.assertNotNull(response.getElement().element("error")
        .element(XMLConstants.POLICY_VIOLATION));
    Assert.assertEquals(XMLConstants.INVALID_NODE, error.getApplicationConditionName());
    Assert.assertEquals(Buddycloud.NS_ERROR, error.getApplicationConditionNamespaceURI());

  }

  @Test
  public void ifPacketCanNotBeProcessedShouldReceiveAFeatureNotImplementedError() throws Exception {
    IQ request = readStanzaAsIq("/iq/pubsub/good-actor.stanza");

    pubsubSet.process(request);
    Assert.assertEquals(1, queue.size());
    IQ response = (IQ) queue.poll();

    Assert.assertEquals(IQ.Type.error, response.getType());

    PacketError error = response.getError();
    Assert.assertNotNull(error);

    Assert.assertEquals(PacketError.Type.cancel, error.getType());
    Assert.assertEquals(PacketError.Condition.feature_not_implemented, error.getCondition());
  }

}