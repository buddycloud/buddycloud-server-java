package org.buddycloud.channelserver.packetprocessor.iq.namespace.register;

import static org.junit.Assert.*;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.dom4j.Element;
import org.dom4j.QName;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class RegisterSetTest extends IQTestHandler {

	private static final String REGISTER_REQUEST_STANZA = "/iq/pubsub/register/register.stanza";
	private static final String REGISTER_REQUEST_CHANNELS_DOMAIN = "channels.server1";
	private static final String REGISTER_REQUEST_DOMAIN = "server1";

	/*
	 * Class under test
	 */
	private RegisterSet registerSet;

	@Mock
	private ChannelManager channelManagerMock;

	@Mock
	private Configuration configuration;

	private LinkedBlockingQueue<Packet> queue;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);

		Mockito.when(channelManagerMock.isLocalNode(Mockito.anyString()))
				.thenReturn(true);

		queue = new LinkedBlockingQueue<Packet>();
		registerSet = new RegisterSet(configuration, queue, channelManagerMock);

		when(configuration.getProperty("server.domain")).thenReturn(
				REGISTER_REQUEST_DOMAIN);

		when(configuration.getServerChannelsDomain()).thenReturn(
				REGISTER_REQUEST_CHANNELS_DOMAIN);
		when(configuration.getServerDomain()).thenReturn(
				REGISTER_REQUEST_DOMAIN);
	}

	@SuppressWarnings("serial")
	@Test
	public void testRegisterNewUserAutoSubscribesLocalChannels()
			throws Exception {
		IQ request = readStanzaAsIq(REGISTER_REQUEST_STANZA);

		when(channelManagerMock.nodeExists(anyString())).thenReturn(false);

		when(configuration.getAutosubscribeChannels()).thenReturn(
				new ArrayList<JID>() {
					{
						add(new JID("channel1@server1"));
						add(new JID("channel2@server1"));
					}
				});

		when(channelManagerMock.isLocalJID(new JID("channel1@server1")))
				.thenReturn(true);
		when(channelManagerMock.isLocalJID(new JID("channel2@server1")))
				.thenReturn(true);

		registerSet.process(request);

		// Check that there are subscribe stanzas in the outqueue
		ArrayList<Packet> packets = new ArrayList<Packet>();
		queue.drainTo(packets);

		assertPacketsContainsSubscribe(packets, request.getFrom(),
				"/user/channel1@server1/posts");
		assertPacketsContainsSubscribe(packets, request.getFrom(),
				"/user/channel2@server1/posts");
	}

	@SuppressWarnings("serial")
	@Test
	public void testRegisterNewUserAutoSubscribesRemoteChannels()
			throws Exception {
		IQ request = readStanzaAsIq(REGISTER_REQUEST_STANZA);

		when(channelManagerMock.nodeExists(anyString())).thenReturn(false);

		when(configuration.getAutosubscribeChannels()).thenReturn(
				new ArrayList<JID>() {
					{
						add(new JID("channel1@server2"));
						add(new JID("channel2@server2"));
					}
				});

		when(channelManagerMock.isLocalJID(new JID("channel1@server2")))
				.thenReturn(false);
		when(channelManagerMock.isLocalJID(new JID("channel2@server2")))
				.thenReturn(false);

		registerSet.process(request);

		// Check that there are subscribe stanzas in the outqueue
		ArrayList<Packet> packets = new ArrayList<Packet>();
		queue.drainTo(packets);

		assertPacketsContainsSubscribe(packets, new JID(
				REGISTER_REQUEST_CHANNELS_DOMAIN),
				"/user/channel1@server2/posts", request.getFrom().toBareJID());
		assertPacketsContainsSubscribe(packets, new JID(
				REGISTER_REQUEST_CHANNELS_DOMAIN),
				"/user/channel2@server2/posts", request.getFrom().toBareJID());
	}

	@Test
	public void testRegisterExistingUserDoesntAutoSubscribeChannels()
			throws Exception {
		IQ request = readStanzaAsIq(REGISTER_REQUEST_STANZA);

		when(channelManagerMock.nodeExists(anyString())).thenReturn(true);

		when(
				channelManagerMock.nodeExists(Conf
						.getPostChannelNodename(request.getFrom())))
				.thenReturn(true);

		registerSet.process(request);

		// Check that there are subscribe stanzas in the outqueue
		ArrayList<Packet> packets = new ArrayList<Packet>();
		queue.drainTo(packets);

		assertPacketsDoesntContainSubscribe(packets, request.getFrom(),
				"/user/channel1@server1/posts");
		assertPacketsDoesntContainSubscribe(packets, request.getFrom(),
				"/user/channel2@server1/posts");
	}

	private void assertPacketsContainsSubscribe(
			final Collection<Packet> packets, final JID jid, final String nodeId) {
		assertPacketsContainsSubscribe(packets, jid, nodeId, null);
	}

	private void assertPacketsContainsSubscribe(
			final Collection<Packet> packets, final JID jid,
			final String nodeId, String actor) {
		for (Packet packet : packets) {
			if (packet.getElement().getName().equals("iq")
					&& packet.getFrom().equals(jid)) {
				Element pubsubEl = packet.getElement().element(
						QName.get("pubsub", JabberPubsub.NAMESPACE_URI));

				if (pubsubEl == null) {
					continue;
				}

				Element subscribeEl = pubsubEl.element("subscribe");

				if (subscribeEl.attributeValue("node").equals(nodeId)) {

					if (actor == null) {
						if (subscribeEl.attributeValue("jid").equals(
								jid.toBareJID().toString())) {
							return;
						}
					} else {

						Element actorEl = pubsubEl.element("actor");

						if (actorEl.getText().equals(actor)
								&& subscribeEl.attributeValue("jid").equals(
										actor)) {
							return;
						}
					}
				}
			}
		}

		if (actor == null) {
			fail("Subscribe to node " + nodeId + " from jid " + jid
					+ " not found");
		} else {
			fail("Subscribe to node " + nodeId + " from jid " + jid
					+ " with actor " + actor + " not found");
		}
	}

	private void assertPacketsDoesntContainSubscribe(
			final Collection<Packet> packets, final JID jid,
			final String nodeId, String actor) {
		for (Packet packet : packets) {
			if (packet.getElement().getName().equals("iq")
					&& packet.getFrom().equals(jid)) {
				Element pubsubEl = packet.getElement().element(
						QName.get("pubsub", JabberPubsub.NAMESPACE_URI));

				if (pubsubEl == null) {
					continue;
				}

				Element subscribeEl = pubsubEl.element("subscribe");

				if (subscribeEl.attributeValue("node").equals(nodeId)) {

					if (actor == null) {
						if (subscribeEl.attributeValue("jid").equals(
								jid.toBareJID().toString())) {
							fail("Subscribe to node " + nodeId + " from jid " + jid
									+ " was found but not expected");
						}
					} else {

						Element actorEl = pubsubEl.element("actor");

						if (actorEl.getText().equals(actor)
								&& subscribeEl.attributeValue("jid").equals(
										actor)) {
							fail("Subscribe to node " + nodeId + " from jid " + jid
									+ " with actor " + actor
									+ " was found but not expected");
						}
					}
				}
			}
		}
	}

	private void assertPacketsDoesntContainSubscribe(
			final Collection<Packet> packets, final JID jid, final String nodeId) {
		assertPacketsDoesntContainSubscribe(packets, jid, nodeId, null);
	}
}
