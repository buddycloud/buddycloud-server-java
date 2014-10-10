package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.ArrayList;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class AffiliationProcessorTest extends IQTestHandler {
    private Message message;
    private AffiliationProcessor affiliationProcessor;
    private Element affiliation;
    private Element affiliations;
    
    private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
    private ChannelManager channelManager;

    private JID jid = new JID("juliet@shakespeare.lit");

    @Before
    public void setUp() throws Exception {

        Properties configuration = new Properties();
        configuration.setProperty("server.domain.channels",
                "channels.shakespeare.lit");

        channelManager = Mockito.mock(ChannelManager.class);
        Configuration.getInstance().remove(
                Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
        
        ArrayList<NodeMembership> subscribers = new ArrayList<NodeMembership>();
        subscribers.add(new NodeMembershipImpl(
                "/user/romeo@shakespeare.lit/posts", jid,
                Subscriptions.subscribed, Affiliations.member, null));
        Mockito.doReturn(new ResultSetImpl<NodeMembership>(subscribers))
                .when(channelManager).getNodeMemberships(Mockito.anyString());

        affiliationProcessor = new AffiliationProcessor(queue, configuration,
                channelManager);

        message = new Message();
        message.setType(Message.Type.headline);
        Element event = message.addChildElement("event",
                JabberPubsub.NS_PUBSUB_EVENT);

        affiliations = event.addElement("affiliations");
        affiliations.addAttribute("node", "/user/juliet@denmark.lit/posts");
        
        affiliation = affiliations.addElement("affiliation");
        affiliation.addAttribute("jid", "romeo@shakespeare.lit");
        affiliation.addAttribute("affiliation",
                Affiliations.publisher.toString());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testInvalidAffiliationValueThrowsException() throws Exception {
        Message badAffiliationValue = message.createCopy();
        badAffiliationValue.getElement().element("event").element("affiliations")
                .element("affiliation").addAttribute("affiliation", "invalid");
        affiliationProcessor.process(badAffiliationValue);
    }

    @Test
    public void testMissingAffiliationElementDoesNotCauseError()
            throws Exception {
        Message noAffiliationElement = message.createCopy();
        noAffiliationElement.getElement().element("event").element("affiliations")
                .element("affiliation").detach();
        affiliationProcessor.process(noAffiliationElement);
    }

    @Test
    public void testEventForLocalNodeIsIgnored() throws Exception {
        Configuration.getInstance().putProperty(
                Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());
        affiliationProcessor.process(message);
        Assert.assertEquals(0, queue.size());
    }

    @Test(expected = NodeStoreException.class)
    public void testNodeStoreExceptionIsThrownWhenExpected() throws Exception {

        Mockito.doThrow(new NodeStoreException())
                .when(channelManager)
                .setUserAffiliation(Mockito.anyString(),
                        Mockito.any(JID.class), Mockito.any(Affiliations.class));
        affiliationProcessor.process(message);
    }

    @Test
    public void testNotificationsAreSentOutAsExpected() throws Exception {
        Configuration.getInstance().putProperty(
                Configuration.CONFIGURATION_SERVER_DOMAIN, "shakespeare.lit");
        affiliationProcessor.process(message);

        Assert.assertEquals(1, queue.size());
        message.setTo(jid.toString());
        Message received = (Message) queue.poll();
        Assert.assertEquals(message.toString(), received.toString());
    }
}
