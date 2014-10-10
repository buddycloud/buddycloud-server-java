package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.channel.node.configuration.field.NodeTitle;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.dom4j.Element;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeCreateTest extends IQTestHandler {
    private IQ request;
    private ChannelManager channelManager;
    private NodeCreate nodeCreate;
    private JID jid;
    private Element element;
    private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
    private String node = "/user/capulet@shakespeare.lit/posts";

    @Before
    public void setUp() throws Exception {
        channelManager = Mockito.mock(ChannelManager.class);
        Configuration.getInstance().putProperty(
                Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER, Boolean.TRUE.toString());
        
        queue = new LinkedBlockingQueue<Packet>();
        nodeCreate = new NodeCreate(queue, channelManager);
        jid = new JID("juliet@shakespeare.lit");
        request = readStanzaAsIq("/iq/pubsub/channel/create/request.stanza");

        nodeCreate.setServerDomain("shakespeare.lit");

        element = new BaseElement("create");
        element.addAttribute("node", node);
    }

    @Test
    public void testPassingCreateAsElementNameReturnsTrue() {
        Element element = new BaseElement("create");
        Assert.assertTrue(nodeCreate.accept(element));
    }

    @Test
    public void testPassingNotCreateAsElementNameReturnsFalse() {
        Element element = new BaseElement("not-create");
        Assert.assertFalse(nodeCreate.accept(element));
    }

    @Test
    public void testPassingNoNodeResultsInErrorStanza() throws Exception {
        Element element = new BaseElement("create");
        nodeCreate.process(element, jid, request, null);

        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals("nodeid-required", error.getApplicationConditionName());
    }

    @Test
    public void testRequestingAlreadyExistingNodeReturnsErrorStanza()
            throws Exception {

        Mockito.when(
                channelManager
                        .nodeExists("/user/capulet@shakespeare.lit/posts"))
                .thenReturn(true);
        nodeCreate.setChannelManager(channelManager);

        nodeCreate.process(element, jid, request, null);

        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.cancel, error.getType());
        Assert.assertEquals(PacketError.Condition.conflict, error.getCondition());
        /**
         * Add this check back in once Tinder supports xmlns on standard
         * conditions Assert.assertEquals(JabberPubsub.NS_XMPP_STANZAS,
         * error.getApplicationConditionNamespaceURI());
         */
    }

    @Test
    public void testUnauthenticatedUserCanNotCreateNode() throws Exception {
        JID jid = new JID("juliet@anon.shakespeare.lit");

        nodeCreate.process(element, jid, request, null);
        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Condition.forbidden, error.getCondition());
        Assert.assertEquals(PacketError.Type.auth, error.getType());
        /**
         * Add this check back in once Tinder supports xmlns on standard
         * conditions Assert.assertEquals(JabberPubsub.NS_XMPP_STANZAS,
         * error.getApplicationConditionNamespaceURI());
         */
    }

    @Test
    public void testInvalidlyFormattedNodeReturnsError() throws Exception {
        element.addAttribute("node", "/user/capulet@shakespeare/posts/invalid");

        nodeCreate.process(element, jid, request, null);
        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
        /**
         * Add this check back in once Tinder supports xmlns on standard
         * conditions Assert.assertEquals(JabberPubsub.NS_XMPP_STANZAS,
         * error.getApplicationConditionNamespaceURI());
         */
    }

    @Test
    public void testNewNodeMustBeOnADomainSupportedByCurrentServer()
            throws Exception {
        element.addAttribute("node", "/user/capulet@shakespearelit/posts");

        nodeCreate.setTopicsDomain("topics.shakespeare.lit");

        nodeCreate.process(element, jid, request, null);
        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals(PacketError.Condition.not_acceptable, error.getCondition());
        /**
         * Add this check back in once Tinder supports xmlns on standard
         * conditions Assert.assertEquals(JabberPubsub.NS_XMPP_STANZAS,
         * error.getApplicationConditionNamespaceURI());
         */
    }

    @Test
    public void testchannelManagerFailureReturnsInternalServerErrorResponse()
            throws Exception {
        Mockito.doThrow(new NodeStoreException())
                .when(channelManager)
                .createNode(Mockito.any(JID.class), Mockito.anyString(),
                        Mockito.anyMapOf(String.class, String.class));
        nodeCreate.setChannelManager(channelManager);
        Helper helper = Mockito.mock(Helper.class);
        Mockito.doReturn(true).when(helper).isValid();
        nodeCreate.setConfigurationHelper(helper);

        nodeCreate.process(element, jid, request, null);
        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);

        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Condition.internal_server_error,
                error.getCondition());
        Assert.assertEquals(PacketError.Type.wait, error.getType());
        /**
         * Add this check back in once Tinder supports xmlns on standard
         * conditions Assert.assertEquals(JabberPubsub.NS_XMPP_STANZAS,
         * error.getApplicationConditionNamespaceURI());
         */
    }

    @Test
    public void testValidCreateNodeRequestReturnsConfirmationStanza()
            throws Exception {
        Helper helper = Mockito.mock(Helper.class);
        Mockito.doReturn(true).when(helper).isValid();
        nodeCreate.setConfigurationHelper(helper);

        nodeCreate.process(element, jid, request, null);
        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
        String error = null;
        try {
            error = response.getError().toString();
            Assert.fail("Unexpected error response");
        } catch (NullPointerException e) {
            Assert.assertNull(error);
        }
        Assert.assertEquals(IQ.Type.result.toString(), response.getElement()
                .attribute("type").getValue());
    }

    @Test
    public void testCreateNodeWithConfigurationResultsInExpectedConfig()
            throws Exception {
        String channelTitle = "test-channel-name";

        HashMap<String, String> configurationProperties = new HashMap<String, String>();
        configurationProperties.put(NodeTitle.FIELD_NAME, channelTitle);

        Helper helper = Mockito.mock(Helper.class);
        Mockito.when(helper.getValues())
                .thenReturn(configurationProperties);
        Mockito.doReturn(true).when(helper).isValid();

        ChannelManager channelManager = Mockito.mock(ChannelManager.class);
        
        HashMap<String, String> conf = new HashMap<String, String>();
        conf.put(NodeTitle.FIELD_NAME, channelTitle);
        
        Mockito.when(channelManager.getNodeConf(Mockito.anyString())).thenReturn(conf);
        nodeCreate.setChannelManager(channelManager);
        nodeCreate.setConfigurationHelper(helper);

        nodeCreate.process(element, jid, request, null);

        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
        String error = null;
        try {
            error = response.getError().toString();
            Assert.fail("Unexpected error response");
        } catch (NullPointerException e) {
            Assert.assertNull(error);
        }
        Map<String, String> nodeConfiguration = channelManager
                .getNodeConf(node);
        Assert.assertEquals(channelTitle,
                nodeConfiguration.get(NodeTitle.FIELD_NAME));
    }

    @Test
    public void testFailingNodeConfigurationReturnsErrorStanza()
            throws Exception {
        String channelTitle = "test-channel-name";

        HashMap<String, String> configurationProperties = new HashMap<String, String>();
        configurationProperties.put(NodeTitle.FIELD_NAME, channelTitle);

        Helper helper = Mockito.mock(Helper.class);
        Mockito.doThrow(new NodeConfigurationException()).when(helper)
                .parse(request);
        nodeCreate.setConfigurationHelper(helper);

        nodeCreate.process(element, jid, request, null);

        Packet response = queue.poll(100, TimeUnit.MILLISECONDS);
        PacketError error = response.getError();
        Assert.assertNotNull(error);
        Assert.assertEquals(PacketError.Type.modify, error.getType());
        Assert.assertEquals(PacketError.Condition.bad_request, error.getCondition());
        /**
         * Add this check back in once Tinder supports xmlns on standard
         * conditions Assert.assertEquals(JabberPubsub.NS_XMPP_STANZAS,
         * error.getApplicationConditionNamespaceURI());
         */
    }
}
