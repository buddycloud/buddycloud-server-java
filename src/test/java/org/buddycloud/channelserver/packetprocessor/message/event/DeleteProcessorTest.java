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

public class DeleteProcessorTest extends IQTestHandler {
	private Message message;
	private DeleteProcessor deleteProcessor;
	private Element delete;

	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private ChannelManager channelManager;

	private JID jid = new JID("juliet@shakespeare.lit");

	@Before
	public void setUp() throws Exception {

		Properties configuration = new Properties();
		configuration.setProperty("server.domain.channels",
				"chgnnels.shakespeare.lit");

		channelManager = Mockito.mock(ChannelManager.class);
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, "shakespeare.lit");

		ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
		members.add(new NodeMembershipImpl(
				"/user/romeo@denmark.lit/posts", jid,
				Subscriptions.subscribed, Affiliations.member, null));
		Mockito.doReturn(new ResultSetImpl<NodeMembership>(members))
				.when(channelManager).getNodeMemberships(Mockito.anyString());

		deleteProcessor = new DeleteProcessor(queue, configuration,
				channelManager);

		message = new Message();
		message.setType(Message.Type.headline);
		Element event = message.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);

		delete = event.addElement("delete");
		delete.addAttribute("node", "/user/juliet@denmark.lit/posts");
	}

	@Test
	public void testEventForLocalNodeIsIgnored() throws Exception {
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, "denmark.lit");
		deleteProcessor.process(message);
		Assert.assertEquals(0, queue.size());
	}

	@Test(expected = NodeStoreException.class)
	public void testNodeStoreExceptionIsThrownWhenExpected() throws Exception {

		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.deleteNode(Mockito.anyString());
		deleteProcessor.process(message);
	}

	@Test
	public void testNotificationsAreSentOutAsExpected() throws Exception {

		deleteProcessor.process(message);

		Assert.assertEquals(1, queue.size());
		message.setTo(jid.toString());
		Assert.assertEquals(message.toString(), queue.poll().toString());
	}
}