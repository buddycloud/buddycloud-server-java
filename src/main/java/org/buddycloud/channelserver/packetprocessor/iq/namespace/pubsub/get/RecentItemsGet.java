package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.io.StringReader;
import java.util.Date;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class RecentItemsGet extends PubSubElementProcessorAbstract {

    private static final Logger LOGGER = Logger.getLogger(RecentItemsGet.class);
    private static final String NODE_SUFFIX = "/posts";

    private Date maxAge;
    private Integer maxItems;

    private Element pubsub;
    private SAXReader xmlReader;

    // RSM details
    private GlobalItemID firstItemId = null;
    private GlobalItemID lastItemId = null;
    private GlobalItemID afterItemId = null;
    private int maxResults = -1;
    private boolean parentOnly = false;

    public RecentItemsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);
        xmlReader = new SAXReader();

        acceptedElementName = XMLConstants.RECENT_ITEMS_ELEM;
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

        if (!isValidStanza()) {
            outQueue.put(response);
            return;
        }

        if (!Configuration.getInstance().isLocalJID(request.getFrom())) {
            response.getElement().addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR, Boolean.FALSE.toString());
        }
        pubsub = response.getElement().addElement(XMLConstants.PUBSUB_ELEM, JabberPubsub.NAMESPACE_URI);
        try {
            parseRsmElement();
            addRecentItems();
            addRsmElement();
            outQueue.put(response);
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            response.getElement().remove(pubsub);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
        }
        outQueue.put(response);
    }

    @Override
    protected boolean parseRsmElement() {
        if (null == resultSetManagement) {
            return true;
        }

        Element max = null;
        Element after = null;
        if (null != (max = resultSetManagement.element("max"))) {
            maxResults = Integer.parseInt(max.getTextTrim());
        }

        if (null != (after = resultSetManagement.element("after"))) {
            try {
                afterItemId = GlobalItemIDImpl.fromBuddycloudString(after.getTextTrim());
            } catch (IllegalArgumentException e) {
                LOGGER.error(e);
                createExtendedErrorReply(Type.modify, Condition.bad_request, "Could not parse the 'after' id: " + after);
                return false;
            }
        }

        return true;
    }

    @Override
    protected void addRsmElement() throws NodeStoreException {
        if (null == firstItemId) {
            return;
        }
        Element rsm = pubsub.addElement("set", NS_RSM);
        rsm.addElement("first", NS_RSM).setText(firstItemId.toString());
        rsm.addElement("last", NS_RSM).setText(lastItemId.toString());

        rsm.addElement("count", NS_RSM).setText(String.valueOf(channelManager.getCountRecentItems(actor, maxAge, maxItems, NODE_SUFFIX, parentOnly)));
    }

    @Override
    protected void addRecentItems() throws NodeStoreException {
        CloseableIterator<NodeItem> items = channelManager.getRecentItems(actor, maxAge, maxItems, maxResults, afterItemId, NODE_SUFFIX, parentOnly);
        String lastNodeId = "";
        Element itemsElement = null;
        while (items.hasNext()) {
            NodeItem item = items.next();
            if (!item.getNodeId().equals(lastNodeId)) {
                itemsElement = pubsub.addElement(XMLConstants.ITEMS_ELEM);
                itemsElement.addAttribute(XMLConstants.NODE_ATTR, item.getNodeId());
                lastNodeId = item.getNodeId();
            }
            try {
                Element entry = xmlReader.read(new StringReader(item.getPayload())).getRootElement();
                Element itemElement = itemsElement.addElement(XMLConstants.ITEM_ELEM);
                itemElement.addAttribute(XMLConstants.ID_ATTR, item.getId());

                if (null == firstItemId) {
                    firstItemId = new GlobalItemIDImpl(null, item.getNodeId(), item.getId());
                }
                lastItemId = new GlobalItemIDImpl(null, item.getNodeId(), item.getId());
                itemElement.add(entry);
            } catch (DocumentException e) {
                LOGGER.error("Error parsing a node entry, ignoring. " + item.getId());
            }
        }
    }

    @Override
    protected boolean isValidStanza() {
        boolean valid = false;
        String failureReason = null;

        Element recentItems = request.getChildElement().element(acceptedElementName);
        try {
            String max = recentItems.attributeValue(XMLConstants.MAX_ATTR);
            if (null != max) {
                maxItems = Integer.parseInt(max);

                String since = recentItems.attributeValue(XMLConstants.SINCE_ATTR);
                if (null != since) {
                    maxAge = Conf.parseDate(since);
                    valid = true;
                } else {
                    failureReason = XMLConstants.SINCE_REQUIRED_ELEM;
                }
            } else {
                failureReason = XMLConstants.MAX_REQUIRED_ELEM;
            }

            String parentOnlyAttribute = recentItems.attributeValue(XMLConstants.PARENT_ONLY_ATTR);
            if ((null != parentOnlyAttribute) && ((Boolean.TRUE.toString().equals(parentOnlyAttribute)) || ("1".equals(parentOnlyAttribute)))) {
                parentOnly = true;
            }


        } catch (NumberFormatException e) {
            failureReason = XMLConstants.INVALID_MAX_VALUE_PROVIDED_ELEM;
            LOGGER.error(e);
        } catch (IllegalArgumentException e) {
            failureReason = XMLConstants.INVALID_SINCE_VALUE_PROVIDED_ELEM;
            LOGGER.error(e);
        }

        if (!valid) {
            createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, failureReason);
        }

        return valid;
    }
}
