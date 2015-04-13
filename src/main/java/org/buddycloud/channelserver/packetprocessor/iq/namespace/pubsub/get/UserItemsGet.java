package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.io.StringReader;
import java.util.Date;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class UserItemsGet extends PubSubElementProcessorAbstract {

    public static final Logger LOGGER = Logger.getLogger(UserItemsGet.class);

    private Date maxAge;

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
                createExtendedErrorReply(Type.modify, Condition.bad_request, "Could not parse the 'after' id: " + after.getTextTrim());
                return false;
            }
        }

        return true;
    }

    public void addRsmElement() throws NodeStoreException {
        if (null == firstItemId) {
            return;
        }
        Element rsm = pubsub.addElement("set", NS_RSM);
        rsm.addElement("first", NS_RSM).setText(firstItemId.toString());
        rsm.addElement("last", NS_RSM).setText(lastItemId.toString());

        rsm.addElement("count", NS_RSM).setText(String.valueOf(channelManager.getCountUserFeedItems(actor, maxAge, parentOnly)));
    }

    protected void addRecentItems() throws NodeStoreException {
      boolean after = true;
      String rsmItem = afterItemId;
      if (beforeItemId) {
        after = false;
        rsmItem = beforeItemId;
      }
        CloseableIterator<NodeItem> items = channelManager.getUserFeedItems(actor, maxAge, maxResults, rsmItem, after, parentOnly);
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
    public boolean isValidStanza() {
        Element userFeedItems = request.getChildElement().element(acceptedElementName);
        try {
            String since = userFeedItems.attributeValue(XMLConstants.SINCE_ATTR);
            String parentOnlyAttribute = userFeedItems.attributeValue(XMLConstants.PARENT_ONLY_ATTR);
            if ((null != parentOnlyAttribute) && ((Boolean.TRUE.toString().equals(parentOnlyAttribute)) || ("1".equals(parentOnlyAttribute)))) {
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
