package org.buddycloud.channelserver.packetprocessor.message.event;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SubscriptionProcessorTest extends IQTestHandler {
	private Message message;
	private SubscriptionProcessor subscriptionProcessor;
	private Element subscription;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private ChannelManager channelManager;

	private JID jid = new JID("juliet@shakespeare.lit");
	private String node = "/users/juliet@shakespeare.lit/posts";

	@Before
	public void setUp() throws Exception {

		Properties configuration = new Properties();
		configuration.setProperty("server.domain.channels",
				"channels.shakespeare.lit");

		channelManager = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(false);
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);

		ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
		members.add(new NodeMembershipImpl(
				"/users/romeo@shakespeare.lit/posts", jid,
				Subscriptions.subscribed, Affiliations.member, null));
		Mockito.doReturn(new ResultSetImpl<NodeMembership>(members)).when(channelManager)
				.getNodeMemberships(Mockito.anyString());

		subscriptionProcessor = new SubscriptionProcessor(queue, configuration,
				channelManager);

		message = new Message();
		message.setType(Message.Type.headline);
		Element event = message.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		
		subscription = event.addElement("subscription");
		subscription.addAttribute("jid", "romeo@shakespeare.lit");
		subscription
				.addAttribute("node", node );
		subscription.addAttribute("subscription",
				Subscriptions.subscribed.toString());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInvalidSubscriptionValueThrowsException() throws Exception {
		Message badSubscriptionValue = message.createCopy();
		badSubscriptionValue.getElement().element("event")
				.element("subscription")
				.addAttribute("subscription", "invalid");
		subscriptionProcessor.process(badSubscriptionValue);
	}

	@Test
	public void testMissingSubscriptionElementDoesNotCauseError()
			throws Exception {
		Message noSubscriptionElement = message.createCopy();
		noSubscriptionElement.getElement().element("event")
				.element("subscription").detach();
		subscriptionProcessor.process(noSubscriptionElement);
	}

	@Test
	public void testEventForLocalNodeIsIgnored() throws Exception {

		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		subscriptionProcessor.process(message);
		Assert.assertEquals(0, queue.size());
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
	
	@Test
	public void setsInvitedByIfProvided() throws Exception {
		
		Message invite = message.createCopy();
		Element subscription = invite.getElement().element("event").element("subscription");
		subscription.attribute("subscription").detach();
		subscription.addAttribute("subscription", "invited");
		subscription.addAttribute("invited-by", "juliet@shakespeare.lit");
		
		subscriptionProcessor.process(invite);
		
		ArgumentCaptor<NodeSubscription> newSubscription = ArgumentCaptor.forClass(NodeSubscription.class);
		
		verify(channelManager, times(1)).addUserSubscription(newSubscription.capture());
		
		Assert.assertEquals(new JID("juliet@shakespeare.lit"), newSubscription.getValue().getInvitedBy());
		Assert.assertEquals(Subscriptions.invited, newSubscription.getValue().getSubscription());
		Assert.assertEquals(node, newSubscription.getValue().getNodeId());
	}
}