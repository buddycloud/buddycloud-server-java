package org.buddycloud.channelserver.queue;

import java.util.ArrayList;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelsEngineMock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.dom4j.Element;
import org.dom4j.tree.DefaultElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class FederatedQueueManagerTest extends IQTestHandler {

    private FederatedQueueManager queueManager;
    private ChannelsEngineMock channelsEngine;
    private String localServer = "channels.shakespeare.lit";
    private Configuration configuration;

    @Before
    public void setUp() throws Exception {
        channelsEngine = new ChannelsEngineMock();
        Mockito.mock(ChannelManager.class);

        configuration = Mockito.mock(Configuration.class);
        Mockito.when(configuration.getProperty(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN)).thenReturn(localServer);
        Mockito.when(configuration.getProperty(Configuration.DISCOVERY_USE_DNS)).thenReturn("true");
        queueManager = new FederatedQueueManager(channelsEngine, configuration);
    }

    @Test(expected = UnknownFederatedPacketException.class)
    public void testAttemptingToForwardNonExistantFederatedPacketThrowsException() throws Exception {
        IQ packet = new IQ();
        packet.setID("1");
        queueManager.passResponseToRequester(packet);
    }

    @Test
    public void testUnDiscoveredServerSpawnsDiscoItemsRequest() throws Exception {
        IQ packet = new IQ();
        packet.setID("1");
        packet.setFrom(new JID("romeo@montague.lit/street"));
        packet.setTo(new JID("capulet.lit"));
        packet.setType(IQ.Type.get);

        queueManager.process(packet);

        Assert.assertEquals(1, channelsEngine.size());
        Packet outgoingPacket = channelsEngine.poll();

        Assert.assertEquals(localServer, outgoingPacket.getElement().attributeValue("from"));
        Assert.assertEquals(JabberPubsub.NS_DISCO_ITEMS, outgoingPacket.getElement().element("query").getNamespace().getStringValue());
    }

    @Test
    public void testResponseToDiscoItemsRequestResultsInDiscoInfoRequests() throws Exception {

        Element item = new DefaultElement("item");
        item.addAttribute("jid", "channels.capulet.lit");
        ArrayList<Element> items = new ArrayList<Element>();
        items.add(item);

        queueManager.processDiscoItemsResponse(new JID("capulet.lit"), items);

        Assert.assertEquals(1, channelsEngine.size());
        Packet discoInfoRequest = channelsEngine.poll();

        Assert.assertEquals(localServer, discoInfoRequest.getElement().attributeValue("from"));
        Assert.assertEquals("channels.capulet.lit", discoInfoRequest.getElement().attributeValue("to"));
        Assert.assertEquals(JabberPubsub.NS_DISCO_INFO, discoInfoRequest.getElement().element("query").getNamespace().getStringValue());
    }

    @Test
    public void testNotProvidingChannelServerInItemsResponseResultsInErrorStanazaReturn() throws Exception {

        // Attempt to send stanza
        IQ packet = new IQ();
        packet.setID("1:some-request");
        packet.setFrom(new JID("romeo@montague.lit/street"));
        packet.setTo(new JID("capulet.lit"));
        packet.setType(IQ.Type.get);

        queueManager.process(packet);

        // Disco#items fired, passing in items list
        Element item = new DefaultElement("item");
        item.addAttribute("jid", "channels.capulet.lit");
        ArrayList<Element> items = new ArrayList<Element>();
        items.add(item);
        queueManager.processDiscoItemsResponse(new JID("capulet.lit"), items);
        channelsEngine.poll();

        // Response to disco#info with no identities
        queueManager.processDiscoInfoResponse(new JID("channels.capulter.lit"), channelsEngine.poll().getID(), new ArrayList<Element>());

        // Expect error response to original packet
        IQ errorResponse = (IQ) channelsEngine.poll();
        Assert.assertEquals(packet.getID(), errorResponse.getID());
        Assert.assertEquals(IQ.Type.error, errorResponse.getType());
        Assert.assertEquals(localServer, errorResponse.getFrom().toString());

        Assert.assertEquals(PacketError.Type.cancel.toXMPP(), errorResponse.getElement().element("error").attributeValue("type"));
        Assert.assertEquals(PacketError.Condition.item_not_found.toXMPP(), errorResponse.getElement().element("error").element("item-not-found").getName());
        Assert.assertEquals("No pubsub channel service discovered for capulet.lit", errorResponse.getElement().element("error").elementText("text"));
    }

    @Test
    public void testPassingChannelServerIdentifierViaItemsResultsInQueuedPacketSending() throws Exception {
        // Attempt to send stanza
        IQ packet = new IQ();
        packet.setID("1:some-request");
        packet.setFrom(new JID("romeo@montague.lit/street"));
        packet.setTo(new JID("topics.capulet.lit"));
        packet.setType(IQ.Type.get);

        queueManager.process(packet);
        channelsEngine.poll();

        // Pass in items with name="buddycloud-server"
        Element item = new DefaultElement("item");
        item.addAttribute("jid", "channels.capulet.lit");
        item.addAttribute("name", "buddycloud-server");
        ArrayList<Element> items = new ArrayList<Element>();
        items.add(item);
        queueManager.processDiscoItemsResponse(new JID("topics.capulet.lit"), items);

        // Note original packet now sent with remote channel server tag
        Packet originalPacketRedirected = channelsEngine.poll();

        Packet expectedForwaredPacket = packet.createCopy();
        expectedForwaredPacket.setFrom(new JID(localServer));
        expectedForwaredPacket.setTo(new JID("channels.capulet.lit"));
        Assert.assertEquals(expectedForwaredPacket.toXML(), originalPacketRedirected.toXML());
    }

    @Test
    public void testOutgoingFederatedPacketsAreRoutedBackToOriginalSender() throws Exception {

        channelsEngine.clear();

        IQ packet = new IQ();
        packet.setID("1:some-request");
        packet.setFrom(new JID("romeo@montague.lit/street"));
        packet.setTo(new JID("topics.capulet.lit"));
        packet.setType(IQ.Type.get);
        packet.getElement().addAttribute("remote-server-discover", "false");

        queueManager.process(packet.createCopy());
        IQ originalPacketRedirected = (IQ) channelsEngine.poll();

        IQ response = IQ.createResultIQ(originalPacketRedirected);
        queueManager.passResponseToRequester(response);

        Assert.assertEquals(1, channelsEngine.size());
        Packet redirected = channelsEngine.poll();

        Assert.assertEquals(packet.getFrom(), redirected.getTo());
    }

    @Test
    public void testOutgoingFederatedPacketsFromDifferentClientsUsingSameIdAreRoutedBackToOriginalSender() throws Exception {

        channelsEngine.clear();

        IQ clientOnePacket = new IQ();
        clientOnePacket.setID("1:some-request");
        clientOnePacket.setFrom(new JID("romeo@montague.lit/street"));
        clientOnePacket.setTo(new JID("topics.capulet.lit"));
        clientOnePacket.setType(IQ.Type.get);
        clientOnePacket.getElement().addAttribute("remote-server-discover", "false");

        IQ clientTwoPacket = new IQ();
        clientTwoPacket.setID("1:some-request");
        clientTwoPacket.setFrom(new JID("juliet@montague.lit/street"));
        clientTwoPacket.setTo(new JID("topics.capulet.lit"));
        clientTwoPacket.setType(IQ.Type.get);
        clientTwoPacket.getElement().addAttribute("remote-server-discover", "false");

        queueManager.addChannelMap(new JID("topics.capulet.lit"));

        queueManager.process(clientOnePacket.createCopy());
        queueManager.process(clientTwoPacket.createCopy());

        IQ clientOneOriginalPacketRedirected = (IQ) channelsEngine.poll();
        IQ clientTwoOriginalPacketRedirected = (IQ) channelsEngine.poll();

        IQ clientOneResponse = IQ.createResultIQ(clientOneOriginalPacketRedirected);
        queueManager.passResponseToRequester(clientOneResponse);

        IQ clientTwoResponse = IQ.createResultIQ(clientTwoOriginalPacketRedirected);
        queueManager.passResponseToRequester(clientTwoResponse);

        Assert.assertEquals(2, channelsEngine.size());
        Packet clientOneRedirected = channelsEngine.poll();
        Packet clientTwoRedirected = channelsEngine.poll();

        Assert.assertEquals(clientOnePacket.getFrom(), clientOneRedirected.getTo());
        Assert.assertEquals(clientTwoPacket.getFrom(), clientTwoRedirected.getTo());
    }

    @Test
    public void testOutgoingIqPacketsGetIdMapped() throws Exception {
        channelsEngine.clear();

        String originalId = "id:12345";
        IQ packet = new IQ();
        packet.setFrom(new JID("romeo@montague.lit/street"));
        packet.setTo(new JID("topics.capulet.lit"));
        packet.setType(IQ.Type.get);
        packet.setID(originalId);
        packet.getElement().addAttribute("remote-server-discover", "false");

        queueManager.addChannelMap(new JID("topics.capulet.lit"));

        queueManager.process(packet.createCopy());

        IQ packetExternal = (IQ) channelsEngine.poll();

        Assert.assertFalse(originalId.equals(packetExternal.getID()));

        IQ response = IQ.createResultIQ(packetExternal);
        queueManager.passResponseToRequester(response);

        IQ packetInternal = (IQ) channelsEngine.poll();

        Assert.assertTrue(originalId.equals(packetInternal.getID()));
    }

    @Test
    public void testNonIqPacketsDoNotGetIdMapped() throws Exception {
        channelsEngine.clear();

        String originalId = "id:12345";
        Message packet = new Message();
        packet.setFrom(new JID("romeo@montague.lit/street"));
        packet.setTo(new JID("topics.capulet.lit"));
        packet.getElement().addAttribute("remote-server-discover", "false");
        packet.setID(originalId);

        queueManager.addChannelMap(new JID("topics.capulet.lit"));

        queueManager.process(packet.createCopy());

        Message packetExternal = (Message) channelsEngine.poll();

        Assert.assertTrue(originalId.equals(packetExternal.getID()));
    }

    @Test
    public void testIqResultPacketsDontGetIdMapped() throws Exception {
        channelsEngine.clear();

        String originalId = "id:12345";
        IQ packet = new IQ();
        packet.setFrom(new JID("romeo@montague.lit/street"));
        packet.setTo(new JID("topics.capulet.lit"));
        packet.setType(IQ.Type.result);
        packet.setID(originalId);
        packet.getElement().addAttribute("remote-server-discover", "false");

        queueManager.addChannelMap(new JID("topics.capulet.lit"));

        queueManager.process(packet.createCopy());

        IQ packetExternal = (IQ) channelsEngine.poll();

        Assert.assertTrue(originalId.equals(packetExternal.getID()));
    }

    @Test
    public void testIqErrorPacketsDontGetIdMapped() throws Exception {
        channelsEngine.clear();

        String originalId = "id:12345";
        IQ packet = new IQ();
        packet.setFrom(new JID("romeo@montague.lit/street"));
        packet.setTo(new JID("topics.capulet.lit"));
        packet.setType(IQ.Type.error);
        packet.setID(originalId);
        packet.getElement().addAttribute("remote-server-discover", "false");

        queueManager.addChannelMap(new JID("topics.capulet.lit"));

        queueManager.process(packet.createCopy());

        IQ packetExternal = (IQ) channelsEngine.poll();

        Assert.assertTrue(originalId.equals(packetExternal.getID()));
    }
}
