package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.special;

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

public class FirehoseGet extends PubSubElementProcessorAbstract {

    private static final int DEFAULT_MAX_RESULTS = 50;
    private static final Logger LOGGER = Logger.getLogger(FirehoseGet.class);

    private Element pubsub;
    private SAXReader xmlReader;
    private boolean isAdmin = false;

    // RSM details
    private String firstItemId = null;
    private String lastItemId = null;
    private String afterItemId = null;
    private int maxResults = -1;

    public FirehoseGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);
        xmlReader = new SAXReader();

        acceptedElementName = XMLConstants.ITEMS_ELEM;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        response = IQ.createResultIQ(reqIQ);
        request = reqIQ;
        actor = actorJID;
        node = elm.attributeValue(XMLConstants.NODE_ATTR);
        resultSetManagement = rsm;

        if (null == actor) {
            actor = request.getFrom();
        }
        determineAdminUserStatus();

        if (false == Configuration.getInstance().isLocalJID(request.getFrom())) {
            response.getElement().addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR, Boolean.FALSE.toString());
        }

        pubsub = response.getElement().addElement(XMLConstants.PUBSUB_ELEM, JabberPubsub.NAMESPACE_URI);
        try {
            parseRsmElement();
            addItems();
            addRsmElement();
            outQueue.put(response);
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            response.getElement().remove(pubsub);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
        }
        outQueue.put(response);

    }

    private void determineAdminUserStatus() {
        for (JID user : getAdminUsers()) {
            if (user.toBareJID().equals(actor.toBareJID())) {
                isAdmin = true;
                return;
            }
        }
    }

    @Override
    protected void addRsmElement() throws NodeStoreException {
        if (firstItemId == null) {
            return;
        }
        Element rsm = pubsub.addElement(XMLConstants.SET_ELEM);
        rsm.addNamespace("", NS_RSM);
        rsm.addElement("first").setText(firstItemId);
        rsm.addElement("last").setText(lastItemId);
        rsm.addElement("count").setText(String.valueOf(channelManager.getFirehoseItemCount(isAdmin, actor.getDomain())));
    }

    private void addItems() throws NodeStoreException {
        if (-1 == maxResults) {
            maxResults = DEFAULT_MAX_RESULTS;
        }
        CloseableIterator<NodeItem> items = channelManager.getFirehose(maxResults, afterItemId, isAdmin, actor.getDomain());
        String lastNode = "";
        Element itemsElement = null;
        while (items.hasNext()) {
            NodeItem item = items.next();
            if (!item.getNodeId().equals(lastNode)) {
                itemsElement = pubsub.addElement(XMLConstants.ITEMS_ELEM);
                itemsElement.addAttribute("node", item.getNodeId());
                lastNode = item.getNodeId();
            }
            try {
                Element entry = xmlReader.read(new StringReader(item.getPayload())).getRootElement();
                Element itemElement = itemsElement.addElement(XMLConstants.ITEM_ELEM);
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
