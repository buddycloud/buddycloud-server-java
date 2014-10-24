package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
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
    protected Map<String, String> nodeConfiguration;

    protected Element resultSetManagement;
    protected String firstItem;
    protected String lastItem;
    protected int totalEntriesCount;
    protected NodeViewAcl nodeViewAcl;

    private Collection<JID> adminUsers;

    protected String acceptedElementName;

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
        Element error = new DOMElement(XMLConstants.ERROR_ELEM);
        error.addAttribute(XMLConstants.TYPE_ATTR, type.toXMPP());
        error.add(standardError);
        error.add(extraError);
        if (null != text) {
            Element description = new DOMElement(XMLConstants.TEXT_ELEM, new org.dom4j.Namespace("", "urn:ietf:params:xml:ns:xmpp-stanzas"));
            description.setText(text);
            error.add(description);
        }
        response.setChildElement(error);
    }

    protected Document getDocumentHelper() {
        return DocumentHelper.createDocument();
    }

    public boolean accept(Element elm) {
        return acceptedElementName.equals(elm.getName());
    }

    protected boolean checkNodeExists() throws NodeStoreException {
        if ((!Configuration.getInstance().isLocalNode(node)) || !channelManager.nodeExists(node)) {
            setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);

            return false;
        }
        return true;
    }

    protected boolean userCanViewNode() throws NodeStoreException {
        if (getNodeViewAcl().canViewNode(node, channelManager.getNodeMembership(node, actor), getNodeAccessModel(),
                Configuration.getInstance().isLocalJID(actor))) {
            return true;
        }
        NodeAclRefuseReason reason = getNodeViewAcl().getReason();
        createExtendedErrorReply(reason.getType(), reason.getCondition(), reason.getAdditionalErrorElement());
        return false;
    }

    public void setNodeViewAcl(NodeViewAcl acl) {
        nodeViewAcl = acl;
    }

    protected NodeViewAcl getNodeViewAcl() {
        if (null == nodeViewAcl) {
            nodeViewAcl = new NodeViewAcl();
        }
        return nodeViewAcl;
    }

    private AccessModels getNodeAccessModel() {
        if (!nodeConfiguration.containsKey(AccessModel.FIELD_NAME)) {
            return AccessModels.authorize;
        }
        return AccessModels.createFromString(nodeConfiguration.get(AccessModel.FIELD_NAME));
    }

    protected boolean actorIsRegistered() {
        if (actor.getDomain().equals(getServerDomain())) {
            return true;
        }
        setErrorCondition(PacketError.Type.auth, PacketError.Condition.forbidden);
        return false;
    }
}
