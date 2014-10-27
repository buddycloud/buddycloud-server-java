package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo;

import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;

public class DiscoInfoGet implements PacketProcessor<IQ> {

    public static final String ELEMENT_NAME = "query";
    private static final Logger logger = Logger.getLogger(DiscoInfoGet.class);
    private final BlockingQueue<Packet> outQueue;
    private final ChannelManager channelManager;
    private String node;
    private IQ result;
    private IQ requestIq;
    private Element query;
    private Map<String, String> conf;

    public DiscoInfoGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        this.outQueue = outQueue;
        this.channelManager = channelManager;
    }

    @Override
    public void process(IQ reqIQ) throws Exception {

        requestIq = reqIQ;
        result = IQ.createResultIQ(reqIQ);
        Element elm = reqIQ.getChildElement();
        node = elm.attributeValue(XMLConstants.NODE_ATTR);
        query = result.setChildElement(ELEMENT_NAME, JabberDiscoInfo.NAMESPACE_URI);
        if (false == Configuration.getInstance().isLocalJID(requestIq.getFrom())) {
            result.getElement().addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR, Boolean.FALSE.toString());
        }
        if ((node == null) || ("".equals(node))) {
            sendServerDiscoInfo();
            return;
        }
        if (false == Configuration.getInstance().isLocalNode(node) && (false == channelManager.isCachedNode(node))) {
            logger.info("Node " + node + " is remote and not cached so " + "we're going off to get disco#info");
            makeRemoteRequest();
            return;
        }
        conf = channelManager.getNodeConf(node);
        if (conf.isEmpty()) {
            nodeDoesntExistResponse();
            return;
        }
        try {
            sendNodeConfigurationInformation();
        } catch (NodeStoreException e) {
            logger.error(e);
            setErrorResponse(PacketError.Type.wait, PacketError.Condition.internal_server_error);
        }
    }

    private void sendNodeConfigurationInformation() throws Exception {

        TreeMap<String, String> configuration = new TreeMap<String, String>();

        DataForm x = new DataForm(DataForm.Type.result);

        FormField formType = x.addField();
        formType.setType(FormField.Type.hidden);
        formType.setVariable("FORM_TYPE");
        formType.addValue("http://jabber.org/protocol/pubsub#meta-data");

        String value;

        configuration.putAll(conf);
        for (String key : configuration.keySet()) {
            value = configuration.get(key);
            if ((true == key.equals(AccessModel.FIELD_NAME)) && (value.equals(AccessModel.local.toString()))
                    && (false == Configuration.getInstance().isLocalJID(requestIq.getFrom()))) {
                value = AccessModel.authorize.toString();
            }
            x.addField(key, null, null).addValue(value);
        }

        query.addAttribute(XMLConstants.NODE_ATTR, node);
        query.addElement("identity").addAttribute("category", "pubsub").addAttribute("type", "leaf");
        query.addElement("feature").addAttribute("var", "http://jabber.org/protocol/pubsub");

        query.add(x.getElement());

        query.addElement("feature").addAttribute("var", JabberPubsub.NAMESPACE_URI);
        Element identity = query.addElement("identity");
        identity.addAttribute("category", "pubsub");
        identity.addAttribute("type", "leaf");

        logger.trace("Returning DISCO info for node: " + node);
        outQueue.put(result);
    }

    private void nodeDoesntExistResponse() throws InterruptedException {
        /*
         * Not found. Let's return something like this: <iq type='error'
         * from='plays.shakespeare.lit' to='romeo@montague.net/orchard' id='info1'> <query
         * xmlns='http://jabber.org/protocol/disco#info' node='/user/pretty@lady.com/posts'/> <error
         * type='cancel'> <item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/> </error>
         * </iq>
         */
        setErrorResponse(PacketError.Type.cancel, PacketError.Condition.item_not_found);
    }

    private void setErrorResponse(PacketError.Type type, Condition condition) throws InterruptedException {
        result.setChildElement(result.getChildElement().createCopy());
        result.setType(Type.error);
        PacketError pe = new PacketError(condition, type);
        result.setError(pe);
        outQueue.put(result);
    }

    private void sendServerDiscoInfo() throws InterruptedException {
        query.addElement("identity").addAttribute("category", "pubsub").addAttribute("type", "channels");

        query.addElement("identity").addAttribute("category", "pubsub").addAttribute("type", "inbox");

        query.addElement("feature").addAttribute("var", "jabber:iq:register");

        query.addElement("feature").addAttribute("var", "http://jabber.org/protocol/disco#info");

        query.addElement("feature").addAttribute("var", "http://jabber.org/protocol/disco#items");

        query.addElement("feature").addAttribute("var", "jabber:iq:search");

        outQueue.put(result);
    }

    private void makeRemoteRequest() throws InterruptedException {
        requestIq.setTo(new JID(node.split("/")[2]).getDomain());
        outQueue.put(requestIq);
    }
}
