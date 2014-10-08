package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public abstract class PubSubElementProcessorAbstract implements PubSubElementProcessor {
    public static final String NS_RSM = "http://jabber.org/protocol/rsm";

    protected BlockingQueue<Packet> outQueue;
    protected ChannelManager channelManager;
    protected Element element;
    protected IQ response;
    protected IQ request;
    protected JID actor;
    protected String serverDomain;
    protected String topicsDomain;
    protected String node = null;
    protected Helper configurationHelper;

    protected Element resultSetManagement;
    protected String firstItem;
    protected String lastItem;
    protected int totalEntriesCount;

    private Collection<JID> adminUsers;

    public void setOutQueue(BlockingQueue<Packet> outQueue) {
        this.outQueue = outQueue;
    }

    public void setChannelManager(ChannelManager channelManager) {
        this.channelManager = channelManager;
    }

    public void setServerDomain(String domain) {
        serverDomain = domain;
    }

    protected String getServerDomain() {
        if (null == serverDomain) {
            serverDomain = Configuration.getInstance().getProperty("server.domain");
        }
        return serverDomain;
    }

    protected Collection<JID> getAdminUsers() {
        if (null == adminUsers) {
            adminUsers = Configuration.getInstance().getAdminUsers();
        }
        return adminUsers;
    }

    public void setTopicsDomain(String domain) {
        topicsDomain = domain;
    }

    public void setNode(String node) {
        this.node = node;
    }

    protected String getTopicsDomain() {
        if (null == topicsDomain) {
            topicsDomain = Configuration.getInstance().getProperty("server.domain.topics");
        }
        return topicsDomain;
    }

    public void setConfigurationHelper(Helper helper) {
        configurationHelper = helper;
    }

    protected Helper getNodeConfigurationHelper() {
        if (null == configurationHelper) {
            configurationHelper = new Helper(channelManager);
        }
        return configurationHelper;
    }

    protected void setErrorCondition(Type type, Condition condition) {
        if (null == response) {
            response = IQ.createResultIQ(request);
        }
        response.setType(IQ.Type.error);
        PacketError error = new PacketError(condition, type);
        response.setError(error);
    }

    protected void createExtendedErrorReply(Type type, Condition condition, String additionalElement) {
        if ((null != additionalElement) && (additionalElement.indexOf(" ") > -1)) {
            // Its probably an error message!
            createExtendedErrorReply(type, condition, null, null, additionalElement);
            return;
        }
        createExtendedErrorReply(type, condition, additionalElement, JabberPubsub.NS_PUBSUB_ERROR);
    }

    protected void createExtendedErrorReply(Type type, Condition condition, String additionalElement, String additionalNamespace) {
        createExtendedErrorReply(type, condition, additionalElement, additionalNamespace, null);
    }


    protected void createExtendedErrorReply(Type type, Condition condition, String additionalElement, String additionalNamespace, String text) {

        if (null == response) {
            response = IQ.createResultIQ(request);
        }
        response.setType(IQ.Type.error);
        Element standardError = new DOMElement(condition.toXMPP(), new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
        Element extraError = new DOMElement(additionalElement, new org.dom4j.Namespace("", additionalNamespace));
        Element error = new DOMElement("error");
        error.addAttribute("type", type.toXMPP());
        error.add(standardError);
        error.add(extraError);
        if (null != text) {
            Element description = new DOMElement("text", new org.dom4j.Namespace("", "urn:ietf:params:xml:ns:xmpp-stanzas"));
            description.setText(text);
            error.add(description);
        }
        response.setChildElement(error);
    }

    protected Document getDocumentHelper() {
        return DocumentHelper.createDocument();
    }
}
