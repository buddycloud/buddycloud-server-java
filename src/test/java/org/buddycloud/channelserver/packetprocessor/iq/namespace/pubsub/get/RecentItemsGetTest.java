package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.ArrayList;
import java.util.Arrays;
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

public class RecentItemsGetTest extends IQTestHandler {

	private IQ request;
	private RecentItemsGet recentItemsGet;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		channelManager = Mockito.mock(ChannelManager.class);
		
		recentItemsGet = new RecentItemsGet(queue, channelManager);
		
		request = readStanzaAsIq("/iq/pubsub/recent-items/request.stanza");
		element = new BaseElement("recent-items");
	}

	@Test
	public void testPassingRecentItemsAsElementNameReturnsTrue() {
		Assert.assertTrue(recentItemsGet.accept(element));
	}

	@Test
	public void testPassingNotRecentItemsAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-items");
		Assert.assertFalse(recentItemsGet.accept(element));
	}

	@Test
	public void testMissingMaxAttributeReturnsErrorStanza() throws Exception {
		
		request.getChildElement().element("recent-items").addAttribute("max", null);

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("max-required",
				error.getApplicationConditionName());
	}
	
	@Test
	public void testInvalidMaxAttributesReturnsErrorStanza() throws Exception {
		
		request.getChildElement().element("recent-items").addAttribute("max", "three");

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("invalid-max-value-provided",
				error.getApplicationConditionName());
	}
	
	@Test
	public void testMissingSinceAttributeReturnsErrorStanza() throws Exception {
		
		request.getChildElement().element("recent-items").addAttribute("since", null);

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("since-required",
				error.getApplicationConditionName());
	}
	
	@Test
	public void testInvalidSinceAttributesReturnsErrorStanza() throws Exception {
		
		request.getChildElement().element("recent-items").addAttribute("since", "a week ago");

		recentItemsGet.process(element, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("invalid-since-value-provided",
				error.getApplicationConditionName());
	}	

}