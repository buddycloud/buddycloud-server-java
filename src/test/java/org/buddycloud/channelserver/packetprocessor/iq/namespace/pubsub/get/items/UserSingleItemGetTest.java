package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items;

import java.util.Date;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class UserSingleItemGetTest extends IQTestHandler {

	private IQ request;
	private UserItemsGet itemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/francisco@denmark.lit/posts";
	private JID jid = new JID("francisco@denmark.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		itemsGet = new UserItemsGet(queue, channelManager);
		request = readStanzaAsIq("/iq/pubsub/items/requestSingleItem.stanza");
		element = request.getElement().element("pubsub").element("items");

		channelManager = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);
		
		itemsGet.setChannelManager(channelManager);
	}

	@Test
	public void testPassingItemsAsElementNameReturnsTrue() {
		Assert.assertTrue(itemsGet.accept(element));
	}
	
	@Test
	public void testInexistentNode() throws Exception {
		Assert.assertTrue(itemsGet.accept(element));
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void testUnableToReadNode() throws Exception {
		
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}
	
	@Test
	public void testInexistentItem() throws Exception {
		
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member, new Date()));
		
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}
	
	@Test
	public void testExistentItemCheckNamespace() throws Exception {
		
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getUserSubscription(node, jid)).thenReturn(
				new NodeSubscriptionImpl(node, jid, Subscriptions.subscribed));
		Mockito.when(channelManager.getUserAffiliation(node, jid)).thenReturn(
				new NodeAffiliationImpl(node, jid, Affiliations.member, new Date()));
		
		final String itemId = "item1Id";
		NodeItem nodeItem = new NodeItemImpl(node, itemId, new Date(), "<payload/>");
		Mockito.when(channelManager.getNodeItem(node, itemId)).thenReturn(nodeItem);
		
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
		
		Assert.assertNull(response.getError());
		Element pubsubEl = response.getElement().element("pubsub");
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI, pubsubEl.getNamespaceURI());
		Element itemsEl = pubsubEl.element("items");
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI, itemsEl.getNamespaceURI());
		Element itemEl = itemsEl.element("item");
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI, itemEl.getNamespaceURI());
	}
}