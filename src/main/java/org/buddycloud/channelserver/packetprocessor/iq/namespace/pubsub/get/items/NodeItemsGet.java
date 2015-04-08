package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items;

import java.io.StringReader;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class NodeItemsGet extends PubSubElementProcessorAbstract {
    private static final Logger LOGGER = Logger.getLogger(NodeItemsGet.class);

    public static final int MAX_ITEMS_TO_RETURN = 50;

    private ChannelManager channelManager;
    private String node;
    private String firstItem;
    private String lastItem;
    private SAXReader xmlReader;
    private Element entry;
    private IQ reply;
    private Element resultSetManagement;
    private Element element;
    private boolean parentOnly = false;

    private Map<String, String> nodeDetails;

    private int rsmEntriesCount;

    private JID actor;

    private Element items;

    public NodeItemsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setOutQueue(outQueue);
        setChannelManager(channelManager);

        this.acceptedElementName = XMLConstants.ITEMS_ELEM;
    }

    public void setChannelManager(ChannelManager ds) {
        channelManager = ds;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        request = reqIQ;

        reply = IQ.createResultIQ(reqIQ);
        element = elm;
        resultSetManagement = rsm;

        if (!Configuration.getInstance().isLocalJID(request.getFrom())) {
            reply.getElement().addAttribute(XMLConstants.REMOTE_SERVER_DISCOVER_ATTR, Boolean.FALSE.toString());
        }

        try {
                    
          if (!isValidStanza()) {
            outQueue.put(reply);
            return;
          }
          
          boolean isCached = channelManager.isCachedNode(node);

          this.actor = actorJID;
          if (null == this.actor) {
              this.actor = request.getFrom();
          }

          if (!Configuration.getInstance().isLocalNode(node) && !isCached) {
              LOGGER.debug("Node " + node + " is remote and not cached, off to get some data");

              makeRemoteRequest();
              return;
          }
          
            if (!checkNodeExists()) {
                setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
                outQueue.put(reply);
                return;
            }

            if (!userCanViewNode()) {
                outQueue.put(reply);
                return;
            }
            xmlReader = new SAXReader();
            if (null == items.element(XMLConstants.ITEM_ELEM)) {
                getItems();
            } else {
                if (!getItem()) {
                    return;
                }
            }
        } catch (NodeStoreException e) {
            LOGGER.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
        } catch (NullPointerException e) {
          LOGGER.error(e);
          setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
        }
        outQueue.put(reply);
    }
    
    protected boolean isValidStanza() {
      Element pubsub = request.getElement().element(XMLConstants.PUBSUB_ELEM);
      items = pubsub.element(XMLConstants.ITEMS_ELEM);
      node = items.attributeValue(XMLConstants.NODE_ATTR);
      return true;
    }

    private boolean getItem() throws Exception {
        NodeItem nodeItem = channelManager.getNodeItem(node, items.element(XMLConstants.ITEM_ELEM).attributeValue(XMLConstants.ID_ATTR));
        if (nodeItem == null) {
            if (!Configuration.getInstance().isLocalNode(node)) {
                makeRemoteRequest();
                return false;
            }
            setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
            return true;
        }

        Element pubsub = reply.getElement().addElement(XMLConstants.PUBSUB_ELEM, JabberPubsub.NAMESPACE_URI);
        Element items = pubsub.addElement(XMLConstants.ITEMS_ELEM).addAttribute(XMLConstants.NODE_ATTR, node);

        addItemToResponse(nodeItem, items);
        return true;
    }

    @Override
    protected void makeRemoteRequest() throws InterruptedException {
        request.setTo(new JID(node.split("/")[2]).getDomain());
        if (null == request.getElement().element(XMLConstants.PUBSUB_ELEM).element(XMLConstants.ACTOR_ELEM)) {
            Element actor = request.getElement().element(XMLConstants.PUBSUB_ELEM).addElement(XMLConstants.ACTOR_ELEM, Buddycloud.NS);
            actor.addText(request.getFrom().toBareJID());
        }
        outQueue.put(request);
    }

    @Override
    protected boolean checkNodeExists() throws NodeStoreException {

        if (channelManager.nodeExists(node)) {
            nodeDetails = channelManager.getNodeConf(node);
            return true;
        }
        setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
        return false;
    }

    @Override
    protected void setErrorCondition(Type type, Condition condition) {
        reply.setType(IQ.Type.error);
        PacketError error = new PacketError(condition, type);
        reply.setError(error);
    }

    private void getItems() throws Exception {
        Element pubsub = new DOMElement(XMLConstants.PUBSUB_ELEM, new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));

        int maxItemsToReturn = MAX_ITEMS_TO_RETURN;
        String afterItemId = null;

        String parentOnlyAttribute = request.getChildElement().element(XMLConstants.ITEMS_ELEM).attributeValue(XMLConstants.PARENT_ONLY_ATTR);
        if ((null != parentOnlyAttribute) && ((Boolean.TRUE.toString().equals(parentOnlyAttribute)) || ("1".equals(parentOnlyAttribute)))) {
            parentOnly = true;
        }
        
        String maxItems = items.attributeValue(XMLConstants.MAX_ITEMS_ATTR);

        if (maxItems != null) {
            maxItemsToReturn = Integer.parseInt(maxItems);
        }

        if (resultSetManagement != null) {
            Element max = resultSetManagement.element(XMLConstants.MAX_ELEM);
            if (max != null) {
                maxItemsToReturn = Integer.parseInt(max.getTextTrim());
            }
            Element after = resultSetManagement.element(XMLConstants.AFTER_ELEM);
            if (after != null) {
                try {
                    // Try and parse it as a global item id
                    GlobalItemID afterGlobalItemID = GlobalItemIDImpl.fromString(after.getTextTrim());
                    afterItemId = afterGlobalItemID.getItemID();

                    // Check it's for the correct node
                    if (!afterGlobalItemID.getNodeID().equals(node)) {
                        createExtendedErrorReply(Type.modify, Condition.item_not_found,
                                "RSM 'after' specifies an unexpected NodeID: " + afterGlobalItemID.getNodeID());
                    }
                } catch (IllegalArgumentException e) {
                    // If the after isn't a valid 'tag:...' then it might just
                    // be a straight ItemID
                    afterItemId = after.getTextTrim();
                    LOGGER.error(e);
                }
            }
        }

        Element items = pubsub.addElement(XMLConstants.ITEMS_ELEM);
        items.addAttribute(XMLConstants.NODE_ATTR, node);

        entry = null;
        int totalEntriesCount = getNodeItems(items, maxItemsToReturn, afterItemId, parentOnly);

        if ((false == Configuration.getInstance().isLocalNode(node)) && (0 == rsmEntriesCount)) {
            LOGGER.debug("No results in cache for remote node, so " + "we're going federated to get more");
            makeRemoteRequest();
            return;
        }

        if ((resultSetManagement != null) || (totalEntriesCount > maxItemsToReturn)) {
            /*
             * TODO(lloydwatkin), add result set here as defined in 6.5.4 Returning Some Items <set
             * xmlns='http://jabber.org/protocol/rsm'> <first
             * index='0'>368866411b877c30064a5f62b917cffe</first>
             * <last>4e30f35051b7b8b42abe083742187228</last> <count>19</count> </set>
             */
            Element rsm = pubsub.addElement(XMLConstants.SET_ELEM, "http://jabber.org/protocol/rsm");

            if (firstItem != null) {
                rsm.addElement(XMLConstants.FIRST_ELEM).setText(firstItem);
                rsm.addElement(XMLConstants.LAST_ELEM).setText(lastItem);
            }
            rsm.addElement(XMLConstants.COUNT_ELEM).setText(Integer.toString(totalEntriesCount));
        }

        reply.setChildElement(pubsub);
    }

    @Override
    protected boolean userCanViewNode() throws NodeStoreException {
        NodeMembership nodeMembership = channelManager.getNodeMembership(node, actor);

        if (getNodeViewAcl().canViewNode(node, nodeMembership, getNodeAccessModel(), Configuration.getInstance().isLocalJID(actor))) {
            return true;
        }
        NodeAclRefuseReason reason = getNodeViewAcl().getReason();
        createExtendedErrorReply(reason.getType(), reason.getCondition(), reason.getAdditionalErrorElement());
        return false;
    }

    private AccessModels getNodeAccessModel() {
        if (!nodeDetails.containsKey(AccessModel.FIELD_NAME)) {
            return AccessModels.authorize;
        }
        return AccessModels.createFromString(nodeDetails.get(AccessModel.FIELD_NAME));
    }

    /**
     * Get items nodes
     */
    private int getNodeItems(Element items, int maxItemsToReturn, String afterItemId, boolean parentOnly) throws NodeStoreException {

        CloseableIterator<NodeItem> itemIt = channelManager.getNodeItems(node, afterItemId, maxItemsToReturn, parentOnly);
        rsmEntriesCount = 0;
        if (itemIt == null) {
            return 0;
        }
        try {
            while (itemIt.hasNext()) {
                ++rsmEntriesCount;
                NodeItem nodeItem = itemIt.next();

                if (firstItem == null) {
                    firstItem = nodeItem.getId();
                }
                addItemToResponse(nodeItem, items);
                lastItem = nodeItem.getId();
            }
            LOGGER.debug("Including RSM there are " + rsmEntriesCount + " items for node " + node);
            return channelManager.countNodeItems(node, parentOnly);
        } finally {
            itemIt.close();
        }
    }

    private void addItemToResponse(NodeItem nodeItem, Element parent) {
        try {
            entry = xmlReader.read(new StringReader(nodeItem.getPayload())).getRootElement();
            Element item = parent.addElement(XMLConstants.ITEM_ELEM);
            item.addAttribute("id", nodeItem.getId());
            item.add(entry);
        } catch (DocumentException e) {
            LOGGER.error("Error parsing a node entry, ignoring. " + nodeItem);
        }
    }

    protected void createExtendedErrorReply(Type type, Condition condition, String additionalElement) {
        reply.setType(IQ.Type.error);
        Element standardError = new DOMElement(condition.toString(), new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
        Element extraError = new DOMElement(additionalElement, new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
        Element error = new DOMElement(XMLConstants.ERROR_ELEM);
        error.addAttribute(XMLConstants.TYPE_ATTR, type.toString());
        error.add(standardError);
        error.add(extraError);
        reply.setChildElement(error);
    }

}
