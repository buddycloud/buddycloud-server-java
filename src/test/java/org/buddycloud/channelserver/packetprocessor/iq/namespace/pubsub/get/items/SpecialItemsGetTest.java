package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
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

public class SpecialItemsGetTest extends IQTestHandler {

	private IQ request;
	private SpecialItemsGet itemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		channelManager = Mockito.mock(ChannelManager.class);

		itemsGet = new SpecialItemsGet(queue, channelManager);

		request = readStanzaAsIq("/iq/pubsub/items/request.stanza");
		element = new BaseElement("items");
	}

	@Test
	public void testPassingItemsAsElementNameReturnsTrue() {
		Assert.assertTrue(itemsGet.accept(element));
	}

	@Test
	public void testPassingNotItemsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		Assert.assertFalse(itemsGet.accept(element));
	}

	@Test
	public void testRequestedUnimplementedSpecialNodeReturnsExpectedError() throws Exception {

		request.getChildElement().element("items")
				.addAttribute("node", "this-is-not-implemented");

		itemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.feature_not_implemented, error.getCondition());
	}
}