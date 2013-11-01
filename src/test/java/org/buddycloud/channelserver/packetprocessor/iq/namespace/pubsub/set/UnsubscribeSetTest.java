package src.test.java.org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.UnsubscribeSet;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class UnsubscribeSetTest extends IQTestHandler {
	private IQ request;
	private UnsubscribeSet unsubscribe;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private String node = "/user/pamela@denmark.lit/posts";
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;

	private NodeSubscription subscription;
	private NodeAffiliation affiliation;

	@Before
	public void setUp() throws Exception {

		channelManager = Mockito.mock(ChannelManager.class);
		Mockito.when(channelManager.isLocalNode(Mockito.anyString()))
				.thenReturn(true);
		Mockito.when(channelManager.nodeExists(Mockito.anyString()))
				.thenReturn(true);

		queue = new LinkedBlockingQueue<Packet>();
		unsubscribe = new UnsubscribeSet(queue, channelManager);
		request = readStanzaAsIq("/iq/pubsub/unsubscribe/request.stanza");
		unsubscribe.setServerDomain("shakespeare.lit");

		element = new BaseElement("unsubscribe");
		element.addAttribute("node", node);

		unsubscribe.setChannelManager(channelManager);

		subscription = new NodeSubscriptionImpl(node, jid,
				Subscriptions.subscribed);
		affiliation = new NodeAffiliationImpl(node, jid, Affiliations.owner,
				new Date());
	}

	@Test
	public void testCanNotUnsubscribeAsOnlyNodeOwner() throws Exception {

		ArrayList<JID> owners = new ArrayList<JID>();
		owners.add(jid);

		Mockito.when(
				channelManager.getUserSubscription(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(subscription);
		Mockito.when(
				channelManager.getUserAffiliation(Mockito.anyString(),
						Mockito.any(JID.class))).thenReturn(affiliation);
		Mockito.when(channelManager.getNodeOwners(Mockito.anyString()))
				.thenReturn(owners);

		unsubscribe.process(element, jid, request, null);

		Assert.assertEquals(1, queue.size());

		IQ response = (IQ) queue.poll();

		Assert.assertEquals(IQ.Type.error, response.getType());

		PacketError error = response.getError();
		Assert.assertNotNull(error);

		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.not_allowed,
				error.getCondition());

	}

}