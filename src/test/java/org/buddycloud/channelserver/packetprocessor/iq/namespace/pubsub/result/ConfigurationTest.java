package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class ConfigurationTest extends IQTestHandler {

	private IQ resultWithNode;
	private IQ resultNoNode;
	private Configuration confResult;
	private Element element;

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		channelManager = Mockito.mock(ChannelManager.class);

		confResult = new Configuration(channelManager);
		resultWithNode = readStanzaAsIq("/iq/pubsub/subscriptions/reply-with-node.stanza");
		resultNoNode = readStanzaAsIq("/iq/pubsub/subscriptions/reply-no-node.stanza");

		element = new BaseElement("configure");
		element.addAttribute("node", node);

		confResult.setChannelManager(channelManager);
	}

	@Test
	public void testPassingConfigureAsElementNameReturnsTrue() {
		Assert.assertTrue(confResult.accept(element));
	}

	@Test
	public void testPassingNotConfigureAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-configure");
		Assert.assertFalse(confResult.accept(element));
	}
	
	@Test(expected = NullPointerException.class)
	public void testInvalidStanzaThrowsException() throws Exception {

		IQ result = toIq("<iq type=\"result\" id=\"subscriptions1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"channels.denmark.lit\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub#owner\" />"
				+ "</iq>");

		confResult.process(element, jid, result, null);
	}
	
	@Test
	public void testEmptyNodeValueCausesNoAction() throws Exception {
		IQ result = toIq("<iq type=\"result\" id=\"subscriptions1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"channels.denmark.lit\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub#owner\">"
				+ "<configure node=\"\" />" + "</pubsub>" + "</iq>");

		confResult.process(element, jid, result, null);

		Mockito.verify(channelManager, Mockito.times(0)).nodeExists(
				Mockito.anyString());
	}
	
	@Test
	public void testIfNodeIsUnknownThenItIsAddedToDatabase() throws Exception {
		IQ result = toIq("<iq type=\"result\" id=\"subscriptions1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"channels.denmark.lit\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub#owner\">"
				+ "<configure node=\"some-node\" />" + "</pubsub>" + "</iq>");

		confResult.process(element, jid, result, null);

		Mockito.when(channelManager.nodeExists(Mockito.anyString())).thenReturn(false);
		Mockito.verify(channelManager, Mockito.times(1)).nodeExists(
				Mockito.anyString());
		Mockito.verify(channelManager, Mockito.times(1)).addRemoteNode(
				Mockito.anyString());
	}
}