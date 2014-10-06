package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class AffiliationsResultTest extends IQTestHandler {

    private IQ resultWithNode;
    private IQ resultNoNode;
    private AffiliationsResult affiliationsResult;
    private Element element;

    private String node = "/user/pamela@denmark.lit/posts";
    private JID jid = new JID("juliet@shakespeare.lit");
    private ChannelManager channelManager;

    @Before
    public void setUp() throws Exception {

        channelManager = Mockito.mock(ChannelManager.class);

        affiliationsResult = new AffiliationsResult(channelManager);
        resultWithNode = readStanzaAsIq("/iq/pubsub/affiliations/reply-with-node.stanza");
        resultNoNode = readStanzaAsIq("/iq/pubsub/affiliations/reply-no-node.stanza");

        element = new BaseElement("affiliations");
        element.addAttribute("node", node);

        affiliationsResult.setChannelManager(channelManager);
        affiliationsResult.setNode(node);
    }

    @Test
    public void testPassingAffiliationsAsElementNameReturnsTrue() {
        Assert.assertTrue(affiliationsResult.accept(element));
    }

    @Test
    public void testPassingNotAffiliationsAsElementNameReturnsFalse() {
        Element element = new BaseElement("not-affiliations");
        Assert.assertFalse(affiliationsResult.accept(element));
    }

    @Test(expected = NullPointerException.class)
    public void testInvalidStanzaThrowsException() throws Exception {

        IQ result =
                toIq("<iq type=\"result\" id=\"affiliations1\" " + "from=\"channels.shakespeare.lit\" " + "to=\"channels.denmark.lit\">"
                        + "<pubsub xmlns=\"http://jabber.org/protocol/pubsub#owner\" />" + "</iq>");

        affiliationsResult.process(element, jid, result, null);
    }

    @Test
    public void testNoAffiliationsCausesNoDatastoreInsert() throws Exception {
        IQ result =
                toIq("<iq type=\"result\" id=\"affiliations1\" " + "from=\"channels.shakespeare.lit\" " + "to=\"channels.denmark.lit\">"
                        + "<pubsub xmlns=\"http://jabber.org/protocol/pubsub#owner\">" + "<affiliations />" + "</pubsub>" + "</iq>");

        affiliationsResult.process(element, jid, result, null);

        Mockito.verify(channelManager, Mockito.times(0)).setUserAffiliation(Mockito.anyString(), Mockito.any(JID.class), Mockito.any(Affiliations.class));
    }

    @Test
    public void testOwnerAffiliationsResultStanzaHandledCorrectly() throws Exception {
        element = new BaseElement("affiliations");
        affiliationsResult.setNode(null);
        affiliationsResult.process(element, jid, resultNoNode, null);

        Mockito.verify(channelManager, Mockito.times(1)).setUserAffiliation(Mockito.eq("/user/pamela@denmark.lit/posts"), Mockito.any(JID.class),
                Mockito.any(Affiliations.class));
        Mockito.verify(channelManager, Mockito.times(1)).setUserAffiliation(Mockito.eq("/user/francisco@denmark.lit/posts"), Mockito.any(JID.class),
                Mockito.any(Affiliations.class));
    }

    @Test
    public void testNodeAffiliationsResultStanzaHandledCorrectly() throws Exception {
        affiliationsResult.process(element, jid, resultWithNode, null);

        Mockito.verify(channelManager, Mockito.times(2)).setUserAffiliation(Mockito.eq(node), Mockito.any(JID.class), Mockito.any(Affiliations.class));
    }
}
