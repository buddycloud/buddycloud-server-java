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
import org.buddycloud.channelserver.pubsub.model.NodeItem;
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

public class ItemsProcessorTest extends IQTestHandler {

	private Message message;
	private ItemsProcessor itemsProcessor;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private ChannelManager channelManager;

	private Element entry;

	@Before
	public void setUp() throws Exception {

		JID jid = new JID("juliet@shakespeare.lit");
		Properties configuration = new Properties();
		configuration.setProperty("server.domain.channels",
				"channels.shakespeare.lit");
		channelManager = Mockito.mock(ChannelManager.class);

		ArrayList<NodeMembership> members = new ArrayList<NodeMembership>();
		members.add(new NodeMembershipImpl(
				"/users/romeo@shakespeare.lit/posts", jid,
				Subscriptions.subscribed, Affiliations.member, null));
		Mockito.doReturn(new ResultSetImpl<NodeMembership>(members))
				.when(channelManager).getNodeMemberships(Mockito.anyString());
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, "shakespeare.lit");

		itemsProcessor = new ItemsProcessor(queue, configuration,
				channelManager);

		message = new Message();
		message.setType(Message.Type.headline);
		Element event = message.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		Element items = event.addElement("items");
		Element item = items.addElement("item");

		entry = item.addElement("entry");
		entry.addNamespace("thr", "http://purl.org/syndication/thread/1.0");
		
		Element updated = entry.addElement("updated");
		updated.setText("2012-10-10T08:37:02.000Z");

		items.addAttribute("node", "/user/romeo@denmark.lit/posts");
		item.addAttribute("id", "publish:1");
		entry.addElement("thr:in-reply-to", "http://purl.org/syndication/thread/1.0").addAttribute("ref", "123455");

	}

	@Test
	public void testLocalNodeEventDoesNotSendNotiifcations() throws Exception {
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, "denmark.lit");
		itemsProcessor.process(message);
		Assert.assertEquals(0, queue.size());
	}

	@Test(expected = NodeStoreException.class)
	public void testNodeStoreExceptionIsThrown() throws Exception {
		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.getNodeMemberships(Mockito.anyString());
		itemsProcessor.process(message);
	}

	@Test(expected = NullPointerException.class)
	public void testConfigurationValueNotSetThrowsException() throws Exception {
		itemsProcessor.setConfiguration(new Properties());
		itemsProcessor.process(message);
	}

	@Test
	public void testNotificationsAreForwarded() throws Exception {
		itemsProcessor.process(message);
		Assert.assertEquals(1, queue.size());
	}

	@Test
	public void testRemoteNodeIsAddedIfNotInDatastore() throws Exception {
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(false);

		itemsProcessor.process(message);

		Mockito.verify(channelManager, Mockito.times(1)).addRemoteNode(
				Mockito.anyString());
	}

	@Test
	public void testItemIsDeletedBeforeAttemptToInsertIntoDatabase()
			throws Exception {
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(false);

		itemsProcessor.process(message);

		Mockito.verify(channelManager, Mockito.times(1)).deleteNodeItemById(
				Mockito.anyString(), Mockito.anyString());
	}

	@Test
	public void testItemsAreCachedToDatastore() throws Exception {
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		itemsProcessor.process(message);

		Mockito.verify(channelManager, Mockito.times(1)).addNodeItem(
				Mockito.any(NodeItem.class));
	}
}