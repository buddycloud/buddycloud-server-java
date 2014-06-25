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
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
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
	private NodeViewAcl nodeViewAcl;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		itemsGet = new UserItemsGet(queue, channelManager);
		request = readStanzaAsIq("/iq/pubsub/items/requestSingleItem.stanza");
		element = request.getElement().element("pubsub").element("items");

		channelManager = Mockito.mock(ChannelManager.class);
		
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);
		
		Mockito.when(channelManager.getNodeMembership(node, jid)).thenReturn(
				new NodeMembershipImpl(node, jid, Subscriptions.subscribed, Affiliations.member));
		nodeViewAcl = Mockito.mock(NodeViewAcl.class);
		Mockito.doReturn(true)
				.when(nodeViewAcl)
				.canViewNode(Mockito.anyString(),
						Mockito.any(NodeMembership.class),
						Mockito.any(AccessModels.class), Mockito.anyBoolean());
		itemsGet.setNodeViewAcl(nodeViewAcl);
		
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
		Packet response = queue.poll();
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void unableToReadNode() throws Exception {
		
		NodeAclRefuseReason reason = Mockito.mock(NodeAclRefuseReason.class);
		Mockito.when(reason.getType()).thenReturn(PacketError.Type.auth);
		Mockito.when(reason.getCondition()).thenReturn(PacketError.Condition.forbidden);
		
		Mockito.doReturn(false)
		.when(nodeViewAcl)
		.canViewNode(Mockito.anyString(),
				Mockito.any(NodeMembership.class),
				Mockito.any(AccessModels.class), Mockito.anyBoolean());
		Mockito.when(nodeViewAcl.getReason()).thenReturn(reason);

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.auth, error.getType());
		Assert.assertEquals(PacketError.Condition.forbidden,
				error.getCondition());
	}
	
	@Test
	public void inexistentItem() throws Exception {

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll();
		
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}
	
	@Test
	public void existentItemCheckNamespace() throws Exception {

		
		final String itemId = "item1Id";
		NodeItem nodeItem = new NodeItemImpl(node, itemId, new Date(), "<payload/>");
		Mockito.when(channelManager.getNodeItem(node, itemId)).thenReturn(nodeItem);
		
		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll();
		
		Assert.assertNull(response.getError());
		Element pubsubEl = response.getElement().element("pubsub");
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI, pubsubEl.getNamespaceURI());
		Element itemsEl = pubsubEl.element("items");
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI, itemsEl.getNamespaceURI());
		Element itemEl = itemsEl.element("item");
		Assert.assertEquals(JabberPubsub.NAMESPACE_URI, itemEl.getNamespaceURI());
	}
}