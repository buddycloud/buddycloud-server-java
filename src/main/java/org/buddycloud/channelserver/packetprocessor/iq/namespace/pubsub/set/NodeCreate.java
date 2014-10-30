package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.channel.node.configuration.field.Creator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeCreate extends PubSubElementProcessorAbstract {
    private static final String NODE_REG_EX = "^/user/[^@]+@[^/]+/[^/]+$";
    private static final String INVALID_NODE_CONFIGURATION = "Invalid node configuration";

    private static final Logger LOGGER = LogManager.getLogger(NodeCreate.class);

    public NodeCreate(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);

        acceptedElementName = XMLConstants.CREATE_ELEM;
    }

    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        element = elm;
        response = IQ.createResultIQ(reqIQ);
        request = reqIQ;
        actor = actorJID;

        node = element.attributeValue(XMLConstants.NODE_ATTR);
        if (null == actorJID) {
            actor = request.getFrom();
        }
        if (!nodePresent()) {
            outQueue.put(response);
            return;
        }
        if (!Configuration.getInstance().isLocalNode(node)) {
            makeRemoteRequest();
            return;
        }
        if ((checkNodeExists()) || (!actorIsRegistered()) || (!nodeHandledByThisServer())) {

            outQueue.put(response);
            return;
        }
        createNode();
    }

    private void createNode() throws InterruptedException {
        try {
            channelManager.createNode(actor, node, getNodeConfiguration());
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
            outQueue.put(response);
            return;
        } catch (NodeConfigurationException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
            outQueue.put(response);
            return;
        }
        response.setType(IQ.Type.result);
        outQueue.put(response);
    }

    private HashMap<String, String> getNodeConfiguration() throws NodeStoreException {
        getNodeConfigurationHelper().parse(request);
        getNodeConfigurationHelper().setNode(node);
        if (!getNodeConfigurationHelper().isValid()) {
            throw new NodeConfigurationException(INVALID_NODE_CONFIGURATION);
        }
        HashMap<String, String> defaultConfiguration = Conf.getDefaultChannelConf(new JID(node.split("/")[2]), actor);
        HashMap<String, String> configuration = getNodeConfigurationHelper().getValues();
        configuration.put(Creator.FIELD_NAME, actor.toBareJID());

        defaultConfiguration.putAll(configuration);

        return defaultConfiguration;
    }

    @Override
    protected boolean checkNodeExists() throws NodeStoreException {
        if (!channelManager.nodeExists(node)) {
            return false;
        }
        setErrorCondition(PacketError.Type.cancel, PacketError.Condition.conflict);
        return true;
    }

    private boolean nodeHandledByThisServer() {
        if (!node.matches(NODE_REG_EX)) {
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
            return false;
        }

        if ((!node.contains("@" + getServerDomain())) && (!node.contains("@" + getTopicsDomain()))) {
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.not_acceptable);
            return false;
        }
        return true;
    }
}
