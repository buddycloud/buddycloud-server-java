package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeConfigureGetTest extends IQTestHandler {

	private NodeConfigureGet configureGet;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	private JID jid = new JID("juliet@shakespeare.lit");
	private ChannelManager channelManager;
	private BaseElement element;

	@Before
	public void setUp() throws Exception {
		queue = new LinkedBlockingQueue<Packet>();
		configureGet = new NodeConfigureGet(queue, channelManager);
		element = new BaseElement("configure");
		channelManager = Mockito.mock(ChannelManager.class);
		configureGet.setChannelManager(channelManager);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());
	}

	@After
	public void tearDown() {
		Mockito.reset(channelManager);
	}

	@Test
	public void testPassingConfigureAsElementNameReturnsTrue() {
		Assert.assertTrue(configureGet.accept(element));
	}

	@Test
	public void testPassingNotConfigureAsElementNameReturnsFalse() {
		Element element = new BaseElement("not-configure");
		Assert.assertFalse(configureGet.accept(element));
	}

	@Test
	public void testMissingNodeAttributeReturnsErrorStanza() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/configure/request-no-node.stanza");
		Element configure = request.getChildElement().element("configure");
		configureGet.process(configure, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals("nodeid-required",
				error.getApplicationConditionName());
	}

	@Test
	public void testInexistentNodeAttributeReturnsErrorStanza()
			throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/configure/request-with-node.stanza");
		Element configure = request.getChildElement().element("configure");

		configureGet.process(configure, jid, request, null);

		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.item_not_found,
				error.getCondition());
	}

	@Test
	public void testRemoteNodeNoError() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/configure/request-with-node.stanza");
		Element configure = request.getChildElement().element("configure");

		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.FALSE.toString());
		configureGet.process(configure, jid, request, null);

		Packet response = queue.poll();

		Assert.assertNull(response.getError());

		Element pubsubResponse = response.getElement().element("pubsub");
		Assert.assertNotNull(pubsubResponse);

		Element configureResponse = pubsubResponse.element("configure");
		Assert.assertNotNull(configureResponse);
		Assert.assertEquals(configure.attributeValue("node"),
				configureResponse.attributeValue("node"));

		Element actor = pubsubResponse.element("actor");
		Assert.assertNotNull(actor);
		Assert.assertEquals(actor.getText(), jid.toBareJID());
	}

	@Test
	public void testLocalNodeNoError() throws Exception {
		IQ request = readStanzaAsIq("/iq/pubsub/configure/request-with-node.stanza");
		Element configure = request.getChildElement().element("configure");

		Map<String, String> conf = new HashMap<String, String>();
		conf.put("pubsub#att1", "value1");
		conf.put("pubsub#att2", "value2");

		String node = configure.attributeValue("node");
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeConf(node)).thenReturn(conf);

		configureGet.process(configure, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNull(error);

		Element pubsubResponse = response.getElement().element("pubsub");
		Assert.assertNotNull(pubsubResponse);

		Element configureResponse = pubsubResponse.element("configure");
		Assert.assertNotNull(configureResponse);
		Assert.assertEquals(node, configureResponse.attributeValue("node"));

		Element x = configureResponse.element("x");
		Assert.assertEquals("http://jabber.org/protocol/pubsub#node_config",
				fieldValue(x, "FORM_TYPE"));
		Assert.assertEquals("value1", fieldValue(x, "pubsub#att1"));
		Assert.assertEquals("value2", fieldValue(x, "pubsub#att2"));
	}

	@Test
	public void testLocalAccessModelGetsReportedAsAuthorizeToRemoveUsers()
			throws Exception {

		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, "denmark.lit");
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);

		IQ request = readStanzaAsIq("/iq/pubsub/configure/request-with-node.stanza");
		Element configure = request.getChildElement().element("configure");

		Map<String, String> conf = new HashMap<String, String>();
		conf.put("pubsub#access_model", AccessModels.local.toString());

		String node = configure.attributeValue("node");
		Mockito.when(channelManager.nodeExists(node)).thenReturn(true);
		Mockito.when(channelManager.getNodeConf(node)).thenReturn(conf);

		configureGet.process(configure, jid, request, null);
		Packet response = queue.poll();

		PacketError error = response.getError();
		Assert.assertNull(error);

		Element pubsubResponse = response.getElement().element("pubsub");
		Assert.assertNotNull(pubsubResponse);

		Element configureResponse = pubsubResponse.element("configure");
		Assert.assertNotNull(configureResponse);
		Assert.assertEquals(node, configureResponse.attributeValue("node"));

		Element x = configureResponse.element("x");
		Assert.assertEquals("http://jabber.org/protocol/pubsub#node_config",
				fieldValue(x, "FORM_TYPE"));
		Assert.assertEquals(AccessModels.authorize.toString(),
				fieldValue(x, "pubsub#access_model"));
	}

	@SuppressWarnings("unchecked")
	private static String fieldValue(Element x, String var) {
		List<Element> elements = x.elements();
		for (Element field : elements) {
			if (field.attributeValue("var").equals(var)) {
				return field.element("value").getText();
			}
		}
		return null;
	}
}