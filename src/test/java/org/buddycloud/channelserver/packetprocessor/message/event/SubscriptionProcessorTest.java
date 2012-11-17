package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class SubscriptionProcessorTest extends IQTestHandler {
	private Message message;
	private SubscriptionProcessor subscriptionProcessor;
	private Element subscription;
	private Element affiliation;

	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private ChannelManager channelManager;
	
	private JID jid = new JID("juliet@shakespeare.lit");

	@Before
	public void setUp() throws Exception {

		Properties configuration = new Properties();
		configuration.setProperty("server.domain.channels", "chgnnels.shakespeare.lit");
		
		channelManager = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(false);
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);
		
		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionImpl(
				"/users/romeo@shakespeare.lit/posts", jid,
				Subscriptions.subscribed));
		Mockito.doReturn(subscribers).when(channelManager)
				.getNodeSubscriptions(Mockito.anyString());
		
		subscriptionProcessor = new SubscriptionProcessor(queue, configuration,
				channelManager);

		message = new Message();
		message.setType(Message.Type.headline);
		Element event = message.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		subscription = event.addElement("subscription");
		affiliation = event.addElement("affiliation");

		subscription.addAttribute("jid", "romeo@shakespeare.lit");
		subscription
				.addAttribute("node", "/users/juliet@shakespeare.lit/posts");
		subscription.addAttribute("subscription",
				Subscriptions.subscribed.toString());

		affiliation.addAttribute("jid", "romeo@shakespeare.lit");
		affiliation.addAttribute("node", "/users/juliet@shakespeare.lit/posts");
		affiliation.addAttribute("affiliation",
				Affiliations.publisher.toString());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInvalidSubscriptionValueThrowsException() throws Exception {
		Message badSubscriptionValue = message.createCopy();
		badSubscriptionValue.getElement().element("event")
				.element("subscription")
				.addAttribute("subscription", "invalid");
		subscriptionProcessor.process(badSubscriptionValue);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInvalidAffiliationValueThrowsException() throws Exception {
		Message badAffiliationValue = message.createCopy();
		badAffiliationValue.getElement().element("event")
				.element("affiliation").addAttribute("affiliation", "invalid");
		subscriptionProcessor.process(badAffiliationValue);
	}

	@Test
	public void testMissingSubscriptionElementDoesNotCauseError() throws Exception {
	    Message noSubscriptionElement = message.createCopy();
	    noSubscriptionElement.getElement().element("event").element("subscription").detach();
	    subscriptionProcessor.process(noSubscriptionElement);
	}

	@Test
	public void testMissingAffiliationElementDoesNotCauseError() throws Exception {
	    Message noAffiliationElement = message.createCopy();
	    noAffiliationElement.getElement().element("event").element("affiliation").detach();
	    subscriptionProcessor.process(noAffiliationElement);
	}
	
	@Test
	public void testEventForLocalNodeIsIgnored() throws Exception {

		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		subscriptionProcessor.process(message);
		Assert.assertEquals(0, queue.size());
	}

	@Test
	public void testSavingSubscriptionShouldNotHappenUnlessUserIsLocal()
			throws Exception {
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(false);
		Mockito.verify(channelManager, Mockito.never()).addUserSubscription(
				Mockito.any(NodeSubscription.class));

		subscriptionProcessor.process(message);
	}

	@Test(expected = NodeStoreException.class)
	public void testNodeStoreExceptionIsThrownWhenExpected() throws Exception {

		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.addUserSubscription(Mockito.any(NodeSubscription.class));
		subscriptionProcessor.process(message);
	}

	@Test
	public void testNotificationsAreSentOutAsExpected() throws Exception {

		subscriptionProcessor.process(message);

		Assert.assertEquals(1, queue.size());
		message.setTo(jid.toString());
		Assert.assertEquals(message.toString(), queue.poll().toString());
	}
}