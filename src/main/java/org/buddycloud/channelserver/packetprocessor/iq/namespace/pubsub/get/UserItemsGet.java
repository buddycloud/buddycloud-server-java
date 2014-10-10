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

public class UserItemsGet extends PubSubElementProcessorAbstract {

    private static final Logger LOGGER = Logger.getLogger(UserItemsGet.class);

    private Date maxAge;

    private Element pubsub;
    private SAXReader xmlReader;

    // RSM details
    private GlobalItemID firstItemId = null;
    private GlobalItemID lastItemId = null;
    private GlobalItemID afterItemId = null;
    private int maxResults = -1;
    private boolean parentOnly = false;

    public UserItemsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setChannelManager(channelManager);
        setOutQueue(outQueue);
        xmlReader = new SAXReader();

        acceptedElementName = XMLConstants.USER_ITEMS;
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
            response.getElement().addAttribute("remote-server-discover", "false");
        }
        pubsub = response.getElement().addElement("pubsub", JabberPubsub.NAMESPACE_URI);
        try {
            if (parseRsmElement()) {
                addRecentItems();
                addRsmElement();
            }
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            response.getElement().remove(pubsub);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
        }
        outQueue.put(response);

    }

    private boolean parseRsmElement() {
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
                createExtendedErrorReply(Type.modify, Condition.bad_request, "Could not parse the 'after' id: " + after.getTextTrim());
                return false;
            }
        }
        return true;
    }

    private void addRsmElement() throws NodeStoreException {
        if (null == firstItemId) {
            return;
        }
        Element rsm = pubsub.addElement("set", NS_RSM);
        rsm.addElement("first", NS_RSM).setText(firstItemId.toString());
        rsm.addElement("last", NS_RSM).setText(lastItemId.toString());

        rsm.addElement("count", NS_RSM).setText(String.valueOf(channelManager.getCountUserFeedItems(actor, maxAge, parentOnly)));
    }

    private void addRecentItems() throws NodeStoreException {
        CloseableIterator<NodeItem> items = channelManager.getUserFeedItems(actor, maxAge, maxResults, afterItemId, parentOnly);
        String lastNodeId = "";
        Element itemsElement = null;
        while (items.hasNext()) {
            NodeItem item = items.next();
            if (!item.getNodeId().equals(lastNodeId)) {
                itemsElement = pubsub.addElement("items");
                itemsElement.addAttribute("node", item.getNodeId());
                lastNodeId = item.getNodeId();
            }
            try {
                Element entry = xmlReader.read(new StringReader(item.getPayload())).getRootElement();
                Element itemElement = itemsElement.addElement("item");
                itemElement.addAttribute("id", item.getId());

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

    private boolean isValidStanza() {
        Element userFeedItems = request.getChildElement().element(XMLConstants.USER_ITEMS);
        try {
            String since = userFeedItems.attributeValue(XMLConstants.SINCE_ATTR);
            String parentOnlyAttribute = userFeedItems.attributeValue(XMLConstants.PARENT_ONLY_ATTR);
            if ((null != parentOnlyAttribute) && (("true".equals(parentOnlyAttribute)) || ("1".equals(parentOnlyAttribute)))) {

                parentOnly = true;
            }

            if (null == since) {
                createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, XMLConstants.SINCE_REQUIRED_ELEM);

                return false;
            }
            maxAge = Conf.parseDate(since);

        } catch (NumberFormatException e) {
            LOGGER.error(e);
            createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, XMLConstants.INVALID_MAX_VALUE_PROVIDED_ELEM);
            return false;
        } catch (IllegalArgumentException e) {
            createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, XMLConstants.INVALID_SINCE_VALUE_PROVIDED_ELEM);
            LOGGER.error(e);
            return false;
        }
        return true;
    }
}
