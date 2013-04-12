package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.apache.commons.lang.StringUtils;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.mock.Mock;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.IQProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliation;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class ItemsResultTest extends IQTestHandler {

	private IQ request;
	private ItemsResult itemsResult;
	private Element element;

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		itemsResult = new ItemsResult(channelManager);
		request = readStanzaAsIq("/iq/pubsub/items/reply.stanza");

		element = new BaseElement("items");
		element.addAttribute("node", node);

		channelManager = Mockito.mock(Mock.class);
		itemsResult.setChannelManager(channelManager);
	}

	@Test
	public void testPassingItemsAsElementNameReturnsTrue() {
		Assert.assertTrue(itemsResult.accept(element));
	}

	@Test
	public void testPassingNotItemsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		Assert.assertFalse(itemsResult.accept(element));
	}

	@Test(expected = NullPointerException.class)
	public void testMissingNodeAttributeThrowsException() throws Exception {
		Element element = new BaseElement("items");
		itemsResult.process(element, jid, request, null);
	}

	@Test
	public void testEnsureResultsComeFromExternalBuddycloudServers()
			throws Exception {
		// If test throws an exception it failed!
		Element element = new BaseElement("items");

		request = toIq("<iq type=\"result\" id=\"items1\" "
				+ "from=\"lloyd@buddycloud.org/junit\" "
				+ "to=\"channels.buddycloud.org\" />");

		itemsResult.process(element, jid, request, null);
	}

	@Test
	public void testNoItemsResultsInNoAdditionsToDatabase() throws Exception {

		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		request = toIq("<iq type=\"result\" id=\"items1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"francisco@denmark.lit/barracks\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub\">"
				+ "<items node=\"/user/francisco@denmark.lit/posts\">"
				+ "</items></pubsub></iq>");

		itemsResult.process(element, jid, request, null);

		Mockito.verify(channelManager, Mockito.never()).addNodeItem(
				Mockito.any(NodeItem.class));
	}

	@Test
	public void testNodeCreatedInDatabaseIfItDoesntExist() throws Exception {

		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(false);

		request = toIq("<iq type=\"result\" id=\"items1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"francisco@denmark.lit/barracks\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub\">"
				+ "<items node=\"/user/francisco@denmark.lit/posts\">"
				+ "</items></pubsub></iq>");

		itemsResult.process(element, jid, request, null);

		Mockito.verify(channelManager, Mockito.times(1)).addRemoteNode(
				Mockito.anyString());
	}

	@Test
	public void testNodeNotCreatedInDatabaseIfAlreadyExists() throws Exception {

		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		request = toIq("<iq type=\"result\" id=\"items1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"francisco@denmark.lit/barracks\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub\">"
				+ "<items node=\"/user/francisco@denmark.lit/posts\">"
				+ "</items></pubsub></iq>");

		itemsResult.process(element, jid, request, null);

		Mockito.verify(channelManager, Mockito.never()).addRemoteNode(
				Mockito.anyString());
	}

	@Test
	public void testItemWithInvalidDateIsNotAddedToDatabase() throws Exception {

		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		request = toIq("<iq type=\"result\" id=\"items1\" "
				+ "from=\"channels.shakespeare.lit\" "
				+ "to=\"francisco@denmark.lit/barracks\">"
				+ "<pubsub xmlns=\"http://jabber.org/protocol/pubsub\">"
				+ "<items node=\"/user/francisco@denmark.lit/posts\">"
				+ "<item id=\"3\">"
				+ "<entry xmlns=\"http://www.w3.org/2005/Atom\" xmlns:activity=\"http://activitystrea.ms/spec/1.0/\">"
				+ "<id>tag:channels.buddycloud.com,/user/koski@buddycloud.com/posts,3</id>"
				+ "<updated>November 5, 1955</updated>" + "<author>"
				+ "<name>koski@buddycloud.com</name>" + "</author>"
				+ "</entry>" + "</item>" + "</items></pubsub></iq>");

		itemsResult.process(element, jid, request, null);

		Mockito.verify(channelManager, Mockito.times(0)).addNodeItem(
				Mockito.any(NodeItem.class));
	}

	@Test
	public void testSendingValidItemsResultsInCorrectNumberOfDatabaseEntries()
			throws Exception {

		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		itemsResult.process(element, jid, request, null);

		Mockito.verify(channelManager, Mockito.times(3)).addNodeItem(
				Mockito.any(NodeItem.class));
	}

	@Test
	public void testSubscriptionsNodeAttemptsToPlaceCorrectDataIntoDatabase()
			throws Exception {

		request = readStanzaAsIq("/iq/pubsub/items/subscriptions-reply.stanza");
		element.addAttribute("node", "/user/pamela@denmark.lit/subscriptions");

		itemsResult.process(element, jid, request, null);

		Mockito.verify(channelManager, Mockito.times(1)).addUserSubscription(
				Mockito.any(NodeSubscription.class));
		Mockito.verify(channelManager, Mockito.times(1)).setUserAffiliation(
				Mockito.anyString(), Mockito.any(JID.class),
				Mockito.any(Affiliations.class));
	}

	@Test
	public void testWhenSubscribedNodeIsntInDatastoreItIsAdded()
			throws Exception {
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(false);

		request = readStanzaAsIq("/iq/pubsub/items/subscriptions-reply.stanza");
		element.addAttribute("node", "/user/pamela@denmark.lit/subscriptions");

		itemsResult.process(element, jid, request, null);

		Mockito.verify(channelManager, Mockito.times(1)).addUserSubscription(
				Mockito.any(NodeSubscription.class));
		Mockito.verify(channelManager, Mockito.times(1)).setUserAffiliation(
				Mockito.anyString(), Mockito.any(JID.class),
				Mockito.any(Affiliations.class));

		Mockito.verify(channelManager, Mockito.times(1)).addRemoteNode(
				"/user/juliet@shakespeare.lit/posts");
	}
}