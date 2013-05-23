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

public class SubscriptionsResultTest extends IQTestHandler {

	private IQ resultWithNode;
	private IQ resultNoNode;
	private SubscriptionsResult subscriptionsResult;
	private Element element;

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		channelManager = Mockito.mock(ChannelManager.class);

		subscriptionsResult = new SubscriptionsResult(channelManager);
		resultWithNode = readStanzaAsIq("/iq/pubsub/subscriptions/reply-with-node.stanza");
		resultNoNode = readStanzaAsIq("/iq/pubsub/subscriptions/reply-no-node.stanza");

		element = new BaseElement("subscriptions");
		element.addAttribute("node", node);

		subscriptionsResult.setChannelManager(channelManager);
	}

	@Test
	public void testPassingSubsriptionsAsElementNameReturnsTrue() {
		Assert.assertTrue(subscriptionsResult.accept(element));
	}

	@Test
	public void testPassingNotSubscriptionsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-subscriptions");
		Assert.assertFalse(subscriptionsResult.accept(element));
	}

	@Test(expected = NullPointerException.class)
	public void testInvalidStanzaThrowsException() throws Exception {

		IQ result = toIq("<iq type=\"result\" id=\"subscriptions1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"channels.denmark.lit\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub#owner\" />"
				+ "</iq>");

		subscriptionsResult.process(element, jid, result, null);
	}

	@Test
	public void testNoSubscriptionsCausesNoDatastoreInsert() throws Exception {
		IQ result = toIq("<iq type=\"result\" id=\"subscriptions1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"channels.denmark.lit\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub#owner\">"
				+ "<subscriptions />" + "</pubsub>" + "</iq>");

		subscriptionsResult.process(element, jid, result, null);

		Mockito.verify(channelManager, Mockito.times(0)).addUserSubscription(
				Mockito.any(NodeSubscription.class));
	}

	@Test
	public void testOwnerSubscriptionsResultStanzaHandledCorrectly()
			throws Exception {
		element = new BaseElement("affiliations");
		subscriptionsResult.process(element, jid, resultNoNode, null);

		Mockito.verify(channelManager, Mockito.times(7)).addUserSubscription(
				Mockito.any(NodeSubscription.class));
	}

	@Test
	public void testNodeSubscriptionsResultStanzaHandledCorrectly()
			throws Exception {
		subscriptionsResult.process(element, jid, resultWithNode, null);

		Mockito.verify(channelManager, Mockito.times(7)).addUserSubscription(
				Mockito.any(NodeSubscription.class));
	}
}