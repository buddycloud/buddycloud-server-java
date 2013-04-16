package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.ArrayList;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
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
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.resultsetmanagement.ResultSet;
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

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionImpl(
				"/users/romeo@shakespeare.lit/posts", jid,
				Subscriptions.subscribed));
		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers))
				.when(channelManager).getNodeSubscriptions(Mockito.anyString());
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(false);

		itemsProcessor = new ItemsProcessor(queue, configuration,
				channelManager);

		message = new Message();
		message.setType(Message.Type.headline);
		Element event = message.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);
		Element items = event.addElement("items");
		Element item = items.addElement("item");

		entry = item.addElement("entry");
		Element updated = entry.addElement("updated");
		updated.setText("2012-10-10T08:37:02Z");

		items.addAttribute("node", "/users/romeo@shakespeare.lit/posts");
		item.addAttribute("id", "publish:1");

	}

	@Test
	public void testLocalNodeEventDoesNotSendNotiifcations() throws Exception {
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		itemsProcessor.process(message);
		Assert.assertEquals(0, queue.size());
	}

	@Test(expected = NodeStoreException.class)
	public void testNodeStoreExceptionIsThrown() throws Exception {
		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.getNodeSubscriptions(Mockito.anyString());
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

	@Test
	public void testWhenRetractElementPassedItemIsDeleted() throws Exception {
		Element item = message.getElement().element("event").element("items")
				.element("item");
		item.remove(entry);
		Element retract = item.addElement("retract");
		retract.addAttribute("id", "1234");

		itemsProcessor.process(message);

		Mockito.verify(channelManager, Mockito.times(1)).deleteNodeItemById(
				Mockito.anyString(), Mockito.anyString());
		Mockito.verify(channelManager, Mockito.times(0)).addNodeItem(
				Mockito.any(NodeItem.class));
	}
}