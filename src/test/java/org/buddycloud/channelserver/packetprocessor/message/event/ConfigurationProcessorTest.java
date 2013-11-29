package org.buddycloud.channelserver.packetprocessor.message.event;

import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
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
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class ConfigurationProcessorTest extends IQTestHandler {
	private Message message;
	private ConfigurationProcessor configurationProcessor;
	private Element configurationElement;
	private Element dataForm;

	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private ChannelManager channelManager;

	private JID jid = new JID("juliet@shakespeare.lit");

	@Before
	public void setUp() throws Exception {

		Properties configuration = new Properties();
		configuration.setProperty("server.domain.channels",
				"chgnnels.shakespeare.lit");

		channelManager = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(false);
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);

		ArrayList<NodeSubscription> subscribers = new ArrayList<NodeSubscription>();
		subscribers.add(new NodeSubscriptionImpl(
				"/users/romeo@shakespeare.lit/posts", jid,
				Subscriptions.subscribed));
		Mockito.doReturn(new ResultSetImpl<NodeSubscription>(subscribers))
				.when(channelManager).getNodeSubscriptions(Mockito.anyString(), Mockito.anyBoolean());

		configurationProcessor = new ConfigurationProcessor(queue,
				configuration, channelManager);

		HashMap<String, String> nodeConfiguration = new HashMap<String, String>();
		nodeConfiguration.put("config1", "value1");

		Helper helper = Mockito.mock(Helper.class);
		Mockito.when(helper.getValues()).thenReturn(nodeConfiguration);
		configurationProcessor.setConfigurationHelper(helper);

		message = new Message();
		message.setType(Message.Type.headline);
		Element event = message.addChildElement("event",
				JabberPubsub.NS_PUBSUB_EVENT);

		configurationElement = event.addElement("configuration");
		configurationElement.addAttribute("jid", "romeo@shakespeare.lit");
		configurationElement.addAttribute("node",
				"/users/juliet@shakespeare.lit/posts");
		dataForm = configurationElement.addElement("x");
		dataForm.addNamespace("", "jabber:x:data");
		dataForm.addAttribute("type", "result");
		Element field = dataForm.addElement("field");
		field.addAttribute("var", "config1");
		Element value = field.addElement("value");
		value.addText("config1");
	}

	@Test
	public void testEventForLocalNodeIsIgnored() throws Exception {

		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		configurationProcessor.process(message);
		Assert.assertEquals(0, queue.size());
	}

	@Test(expected = NodeStoreException.class)
	public void testNodeStoreExceptionIsThrownWhenExpected() throws Exception {

		Mockito.doThrow(new NodeStoreException()).when(channelManager)
				.isLocalNode(Mockito.anyString());
		configurationProcessor.process(message);
	}

	@Test
	public void testRemoteNodeIsCreatedIfNotInDataStore() throws Exception {
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(false);

		configurationProcessor.process(message);

		Mockito.verify(channelManager, Mockito.times(1)).addRemoteNode(
				Mockito.anyString());
	}

	@Test
	public void testExpectedDetailsAreSavedToTheDataStore() throws Exception {
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		configurationProcessor.process(message);

		HashMap<String, String> match = new HashMap<String, String>();
		match.put("config1", "value1");
		
		Mockito.verify(channelManager, Mockito.times(1)).setNodeConf(
				Mockito.anyString(), Mockito.eq(match));
		Mockito.verify(channelManager, Mockito.times(0)).addRemoteNode(
				Mockito.anyString());
	}

	@Test
	public void testNotificationsAreSentOutAsExpected() throws Exception {

		configurationProcessor.process(message);

		Assert.assertEquals(1, queue.size());
		message.setTo(jid.toString());
		Assert.assertEquals(message.toString(), queue.poll().toString());
	}
}