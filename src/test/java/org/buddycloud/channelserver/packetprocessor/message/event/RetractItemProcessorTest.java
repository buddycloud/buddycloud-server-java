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

public class RetractItemProcessorTest extends IQTestHandler {

	private Message message;
	private RetractItemProcessor retractItemProcessor;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		JID jid = new JID("juliet@denmark.lit");
		Properties configuration = new Properties();
		configuration.setProperty("server.domain.channels",
				"channels.shakespeare.lit");
		channelManager = Mockito.mock(ChannelManager.class);

		ArrayList<NodeMembership> subscribers = new ArrayList<NodeMembership>();
		subscribers.add(new NodeMembershipImpl(
				"/user/romeo@shakespeare.lit/posts", jid,
				Subscriptions.subscribed, Affiliations.member, null));
		Mockito.doReturn(new ResultSetImpl<NodeMembership>(subscribers))
				.when(channelManager).getNodeMemberships(Mockito.anyString());
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, "denmark.lit");
        
		retractItemProcessor = new RetractItemProcessor(queue, configuration,
				channelManager);

		message = new Message();
		message.setType(Message.Type.headline);
		Element event = message.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		Element items = event.addElement("items");
		Element retract = items.addElement("retract");

		items.addAttribute("node", "/user/romeo@shakespeare.lit/posts");
		retract.addAttribute("id", "publish:1");

	}

	@Test
	public void testLocalNodeEventDoesNotSendNotiifcations() throws Exception {
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, "shakespeare.lit");

		retractItemProcessor.process(message);
		Assert.assertEquals(0, queue.size());
	}

	@Test(expected = NodeStoreException.class)
	public void testNodeStoreExceptionIsThrown() throws Exception {
		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.getNodeMemberships(Mockito.anyString());
		retractItemProcessor.process(message);
	}

	@Test(expected = NullPointerException.class)
	public void testConfigurationValueNotSetThrowsException() throws Exception {
		retractItemProcessor.setConfiguration(new Properties());
		retractItemProcessor.process(message);
	}

	@Test
	public void testNotificationsAreForwarded() throws Exception {
		retractItemProcessor.process(message);
		Assert.assertEquals(1, queue.size());
	}

	@Test
	public void testWhenRetractElementPassedItemIsDeleted() throws Exception {

		retractItemProcessor.process(message);

		Mockito.verify(channelManager, Mockito.times(1)).deleteNodeItemById(
				Mockito.anyString(), Mockito.anyString());
	}
}