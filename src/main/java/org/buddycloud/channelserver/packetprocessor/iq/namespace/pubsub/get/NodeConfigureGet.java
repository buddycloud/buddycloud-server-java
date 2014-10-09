package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
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

        if (!nodeProvided()) {
            outQueue.put(response);
            return;
        }

        if (!channelManager.isLocalNode(node) && !channelManager.isCachedNodeConfig(node)) {
            makeRemoteRequest();
            return;
        }

        try {
            if (!nodeExists()) {
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
            if ((key.equals(AccessModel.FIELD_NAME)) && (value.equals(AccessModel.local.toString())) && (!channelManager.isLocalJID(actor))) {
                value = AccessModel.authorize.toString();
            }
            x.addField(key, null, null).addValue(value);
        }
        Element pubsub = response.setChildElement(XMLConstants.PUBSUB_ELEM, JabberPubsub.NS_PUBSUB_OWNER);
        Element configure = pubsub.addElement("configure");
        configure.addAttribute("node", node);
        configure.add(x.getElement());
        outQueue.put(response);
    }

    private boolean nodeExists() throws NodeStoreException {
        if (channelManager.nodeExists(node)) {
            return true;
        }
        setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
        return false;
    }

    private boolean nodeProvided() {
        if ((null != node) && !"".equals(node)) {
            return true;
        }
        response.setType(IQ.Type.error);
        Element nodeIdRequired = new DOMElement("nodeid-required", new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
        Element badRequest = new DOMElement(PacketError.Condition.bad_request.toXMPP(), new Namespace("", JabberPubsub.NS_XMPP_STANZAS));
        Element error = new DOMElement("error");
        error.addAttribute("type", "modify");
        error.add(badRequest);
        error.add(nodeIdRequired);
        response.setChildElement(error);
        return false;
    }

    private void makeRemoteRequest() throws InterruptedException {
        request.setTo(new JID(node.split("/")[2]).getDomain());
        Element actor = request.getElement().element(XMLConstants.PUBSUB_ELEM).addElement(XMLConstants.ACTOR_ELEM, Buddycloud.NS);
        actor.addText(request.getFrom().toBareJID());
        outQueue.put(request);
    }
}
