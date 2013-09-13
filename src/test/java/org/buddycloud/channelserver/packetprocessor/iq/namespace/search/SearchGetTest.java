package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
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

public class SearchGetTest extends IQTestHandler {

	private IQ request;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private ChannelManager channelManager;

	private SearchGet search;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		channelManager = Mockito.mock(ChannelManager.class);

		search = new SearchGet(queue, channelManager);

		request = new IQ();
		request.setFrom("romeo@shakespeare.lit/home");
		request.setType(IQ.Type.get);
		request.setTo(new JID("channels.shakespeare.lit"));
		Element query = request.getElement().addElement("query");
		query.addNamespace("", Search.NAMESPACE_URI);

		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);

	}

	@Test
	public void testOnlyAcceptsPacketsFromLocalUsers() throws Exception {

		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(false);

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.not_allowed,
				error.getCondition());
	}
	/*
	 * @Test public void testReturnsInstructionsElement() throws Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormElement() throws Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormTitleElement() throws Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormInstructionsElement() throws
	 * Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormTypeElement() throws Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormContentElement() throws Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormAuthorElement() throws Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormResultsPerPageElement() throws
	 * Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormPageElement() throws Exception {
	 * 
	 * 
	 * }
	 */
}