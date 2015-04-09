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

    public static final Logger LOGGER = Logger.getLogger(RecentItemsGet.class);

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
            pubsub = response.getElement().addElement(XMLConstants.PUBSUB_ELEM, JabberPubsub.NAMESPACE_URI);
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

    protected void addRsmElement() throws NodeStoreException {

        Element rsm = pubsub.addElement(XMLConstants.SET_ELEM);
        rsm.addNamespace("", NS_RSM);
        if (null != firstItemId) {
            rsm.addElement("first").setText(firstItemId);
            rsm.addElement("last").setText(lastItemId);
        }
        rsm.addElement("count").setText(
            String.valueOf(channelManager.getCountNodeItemReplies(node, parentId)));
    }

    private void addReplies() throws NodeStoreException {

        String rsmItem = afterItemId;
        boolean after = true;
        if (null != beforeItemId) {
          rsmItem = beforeItemId;
          after = false;
        }
        
        CloseableIterator<NodeItem> items = channelManager.getNodeItemReplies(node, parentId, rsmItem, after, maxResults);
        NodeItem item;
        Element entry;
        Element itemElement;
        Element itemsElement = pubsub.addElement(XMLConstants.ITEMS_ELEM);
        itemsElement.addAttribute(XMLConstants.NODE_ATTR, node);

        while (items.hasNext()) {
            item = items.next();

            try {
                entry = xmlReader.read(new StringReader(item.getPayload())).getRootElement();
                itemElement = itemsElement.addElement(XMLConstants.ITEM_ELEM);
                itemElement.addAttribute(XMLConstants.ID_ATTR, item.getId());
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
}
