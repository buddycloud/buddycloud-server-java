package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.io.StringReader;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class RepliesGet extends PubSubElementProcessorAbstract {

    private Element pubsub;
    private SAXReader xmlReader;

    // RSM details
    private String firstItemId = null;
    private String lastItemId = null;
    private String afterItemId = null;
    private int maxResults = -1;
    private String parentId;

    private static final Logger LOGGER = Logger.getLogger(RecentItemsGet.class);

    public static final String NS_RSM = "http://jabber.org/protocol/rsm";

    public RepliesGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);

        xmlReader = new SAXReader();

        acceptedElementName = XMLConstants.REPLIES;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        response = IQ.createResultIQ(reqIQ);
        request = reqIQ;
        actor = actorJID;
        resultSetManagement = rsm;

        if (null == actor) {
            actor = request.getFrom();
        }

        if (!isValidStanza()) {
            outQueue.put(response);
            return;
        }

        try {
            if (!Configuration.getInstance().isLocalJID(request.getFrom())) {
                response.getElement().addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR, Boolean.FALSE.toString());
            }
            pubsub = response.getElement().addElement("pubsub", JabberPubsub.NAMESPACE_URI);
            if ((!userCanViewNode()) || (!itemExists())) {
                outQueue.put(response);
                return;
            }
            parseRsmElement();
            addReplies();
            addRsmElement();
            outQueue.put(response);
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            response.getElement().remove(pubsub);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
        }
        outQueue.put(response);

    }

    private boolean itemExists() throws NodeStoreException {
        if (null != channelManager.getNodeItem(node, parentId)) {
            return true;
        }
        setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
        return false;
    }

    private void parseRsmElement() {
        Element rsmElement = request.getChildElement().element("set");
        if (null == rsmElement) {
            return;
        }
        Element max;
        Element after;
        if (null != (max = rsmElement.element("max"))) {
            maxResults = Integer.parseInt(max.getTextTrim());
        }
        if (null != (after = rsmElement.element("after"))) {
            afterItemId = after.getTextTrim();
        }
    }

    private void addRsmElement() throws NodeStoreException {
        if (null == firstItemId) {
            return;
        }
        Element rsm = pubsub.addElement("set");
        rsm.addNamespace("", NS_RSM);
        rsm.addElement("first").setText(firstItemId);
        rsm.addElement("last").setText(lastItemId);
        rsm.addElement("count").setText(String.valueOf(channelManager.getCountNodeItemReplies(node, parentId)));
    }

    private void addReplies() throws NodeStoreException {

        CloseableIterator<NodeItem> items = channelManager.getNodeItemReplies(node, parentId, afterItemId, maxResults);
        NodeItem item;
        Element entry;
        Element itemElement;
        Element itemsElement = pubsub.addElement("items");
        itemsElement.addAttribute("node", node);

        while (items.hasNext()) {
            item = items.next();

            try {
                entry = xmlReader.read(new StringReader(item.getPayload())).getRootElement();
                itemElement = itemsElement.addElement("item");
                itemElement.addAttribute("id", item.getId());
                if (null == firstItemId) {
                    firstItemId = item.getId();
                }
                lastItemId = item.getId();
                itemElement.add(entry);
            } catch (DocumentException e) {
                LOGGER.error("Error parsing a node entry, ignoring. " + item.getId());

            }
        }
    }

    private boolean isValidStanza() {
        Element replies = request.getChildElement().element(XMLConstants.REPLIES);
        try {
            node = replies.attributeValue(XMLConstants.NODE_ATTR);
            if (null == node) {
                createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, XMLConstants.NODE_ID_REQUIRED);

                return false;
            }
            parentId = replies.attributeValue(XMLConstants.ITEM_ID);
            if (null == parentId) {
                createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, XMLConstants.ITEM_ID_REQUIRED);
                return false;
            }
            if (!channelManager.nodeExists(node)) {
                setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);

                return false;
            }
            nodeConfiguration = channelManager.getNodeConf(node);
        } catch (NullPointerException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
            return false;
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
            return false;
        }
        return true;
    }
}
