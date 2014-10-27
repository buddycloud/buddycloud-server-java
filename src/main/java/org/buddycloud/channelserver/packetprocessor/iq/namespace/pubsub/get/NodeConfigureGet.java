package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeConfigureGet extends PubSubElementProcessorAbstract {

    public static final String NS_CONFIGURE = "http://jabber.org/protocol/pubsub#node_config";

    private static final Logger LOGGER = Logger.getLogger(NodeConfigureGet.class);

    public NodeConfigureGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);

        acceptedElementName = XMLConstants.CONFIGURE_ELEM;
    }

    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {

        response = IQ.createResultIQ(reqIQ);
        request = reqIQ;
        actor = actorJID;
        node = elm.attributeValue(XMLConstants.NODE_ATTR);

        if (null == actor) {
            actor = request.getFrom();
        }

        if (!nodePresent()) {
            outQueue.put(response);
            return;
        }

        if (!Configuration.getInstance().isLocalNode(node) && !channelManager.isCachedNodeConfig(node)) {
            makeRemoteRequest();
            return;
        }

        try {
            if (!checkNodeExists()) {
                outQueue.put(response);
                return;
            }
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.cancel, PacketError.Condition.internal_server_error);
            outQueue.put(response);
            return;
        }
        getNodeConfiguration();
    }

    private void getNodeConfiguration() throws Exception {
        Map<String, String> nodeConf = channelManager.getNodeConf(node);

        DataForm x = new DataForm(DataForm.Type.result);

        FormField formType = x.addField();
        formType.setType(FormField.Type.hidden);
        formType.setVariable("FORM_TYPE");
        formType.addValue(NS_CONFIGURE);

        String value;

        for (String key : nodeConf.keySet()) {
            // If access model is 'local' and its not a local user return 'authorize'
            value = nodeConf.get(key);
            if ((key.equals(AccessModel.FIELD_NAME)) && (value.equals(AccessModel.local.toString())) && (!Configuration.getInstance().isLocalJID(actor))) {

                value = AccessModel.authorize.toString();
            }
            x.addField(key, null, null).addValue(value);
        }

        Element pubsub = response.setChildElement(XMLConstants.PUBSUB_ELEM, JabberPubsub.NS_PUBSUB_OWNER);

        Element configure = pubsub.addElement(XMLConstants.CONFIGURE_ELEM);
        configure.addAttribute(XMLConstants.NODE_ATTR, node);
        configure.add(x.getElement());
        outQueue.put(response);
    }
}
