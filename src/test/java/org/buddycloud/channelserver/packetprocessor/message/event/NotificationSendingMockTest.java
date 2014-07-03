package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.ArrayList;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class NotificationSendingMockTest extends IQTestHandler {
	
	private static final String CHANNEL_SERVER = "channels.server.com";

	private Message message;

	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private ChannelManager channelManager;

	private JID jid = new JID("juliet@shakespeare.lit");
	private NotificationSendingMock notificationSending;

	@Before
	public void setUp() throws Exception {

		channelManager = Mockito.mock(ChannelManager.class);

		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);

		Properties configuration = new Properties();
		configuration.setProperty(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN, CHANNEL_SERVER);
		notificationSending = new NotificationSendingMock(channelManager, configuration,
				queue);

		message = new Message();
		message.setType(Message.Type.headline);
		message.getElement().addAttribute("scheme", Integer.toString(1));
	}

	@Test
	public void noNotificationSentForRemoteUser() throws Exception {

		registerUserResponse(Subscriptions.none, Affiliations.none);
		
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
		.thenReturn(false);
		
		notificationSending.process(message);
		Assert.assertEquals(0, queue.size());
	}
	
	@Test
	public void sendsNotificationToSpecifiedUser() throws Exception {
		ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
		Mockito.doReturn(new ResultSetImpl<NodeMembership>(members))
		.when(channelManager).getNodeMemberships(Mockito.anyString());
		
		String user = "user@example.com";
		Message message = this.message.createCopy();
		message.getElement().addAttribute("jid", user);
		
		notificationSending.process(message);
		Assert.assertEquals(1, queue.size());
		Assert.assertEquals(user, queue.poll().getElement().attributeValue("to"));
	}
	
	
	private void registerUserResponse(Subscriptions subscription, Affiliations affiliation) throws Exception {
		ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
		members.add(new NodeMembershipImpl(
				"/users/romeo@shakespeare.lit/posts", jid,
				subscription, affiliation, null));
		Mockito.doReturn(new ResultSetImpl<NodeMembership>(members))
				.when(channelManager).getNodeMemberships(Mockito.anyString());
	}

}