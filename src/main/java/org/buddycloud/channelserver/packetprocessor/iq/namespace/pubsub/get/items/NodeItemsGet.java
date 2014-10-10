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
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
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

public class NodeItemsGet implements PubSubElementProcessor {
    private static final Logger logger = Logger.getLogger(NodeItemsGet.class);

    private final BlockingQueue<Packet> outQueue;

    private ChannelManager channelManager;
    private String node;
    private String firstItem;
    private String lastItem;
    private SAXReader xmlReader;
    private Element entry;
    private IQ requestIq;
    private IQ reply;
    private Element resultSetManagement;
    private Element element;

    private NodeViewAcl nodeViewAcl;
    private Map<String, String> nodeDetails;

    private int rsmEntriesCount;

    private JID actor;

    public NodeItemsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        this.outQueue = outQueue;
        setChannelManager(channelManager);
    }

    public void setChannelManager(ChannelManager ds) {
        channelManager = ds;
    }

    public void setNodeViewAcl(NodeViewAcl acl) {
        nodeViewAcl = acl;
    }

    private NodeViewAcl getNodeViewAcl() {
        if (null == nodeViewAcl) {
            nodeViewAcl = new NodeViewAcl();
        }
        return nodeViewAcl;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        node = elm.attributeValue("node");
        requestIq = reqIQ;
        reply = IQ.createResultIQ(reqIQ);
        element = elm;
        resultSetManagement = rsm;

        if (!Configuration.getInstance().isLocalJID(requestIq.getFrom())) {
            reply.getElement().addAttribute("remote-server-discover", "false");
        }

        boolean isCached = channelManager.isCachedNode(node);

        this.actor = actorJID;
        if (null == this.actor) {
            this.actor = requestIq.getFrom();
        }

        if (!Configuration.getInstance().isLocalNode(node) && !isCached) {
            logger.debug("Node " + node
                    + " is remote and not cached, off to get some data");
            makeRemoteRequest();
            return;
        }

        try {
            if (!nodeExists()) {
                setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
                outQueue.put(reply);
                return;
            }

            if (!userCanViewNode()) {
                outQueue.put(reply);
                return;
            }
            xmlReader = new SAXReader();
            if (element.element("item") == null) {
                getItems();
            } else {
                if (!getItem()) {
                    return;
                }
            }
        } catch (NodeStoreException e) {
            logger.error(e);
            setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
        }
        outQueue.put(reply);
    }

    private boolean getItem() throws Exception {
        NodeItem nodeItem = channelManager.getNodeItem(node,
                element.element("item").attributeValue("id"));
        if (nodeItem == null) {
            if (!Configuration.getInstance().isLocalNode(node)) {
                makeRemoteRequest();
                return false;
            }
            setErrorCondition(PacketError.Type.cancel,
                    PacketError.Condition.item_not_found);
            return true;
        }
        Element pubsub = reply.getElement().addElement("pubsub",
                JabberPubsub.NAMESPACE_URI);
        Element items = pubsub.addElement("items").addAttribute("node", node);
        addItemToResponse(nodeItem, items);
        return true;
    }

    private void makeRemoteRequest() throws InterruptedException {
        requestIq.setTo(new JID(node.split("/")[2]).getDomain());
        if (null == requestIq.getElement().element("pubsub").element("actor")) {
            Element actor = requestIq.getElement().element("pubsub").addElement("actor", Buddycloud.NS);
            actor.addText(requestIq.getFrom().toBareJID());
        }
        outQueue.put(requestIq);
    }

    private boolean nodeExists() throws NodeStoreException {

        if (channelManager.nodeExists(node)) {
            nodeDetails = channelManager.getNodeConf(node);
            return true;
        }
        setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
        return false;
    }

    private void setErrorCondition(Type type, Condition condition) {
        reply.setType(IQ.Type.error);
        PacketError error = new PacketError(condition, type);
        reply.setError(error);
    }

    private void getItems() throws Exception {
        Element pubsub = new DOMElement(PubSubGet.ELEMENT_NAME, new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));

        int maxItemsToReturn = MAX_ITEMS_TO_RETURN;
        String afterItemId = null;

        String maxItems = element.attributeValue("max_items");
        if (maxItems != null) {
            maxItemsToReturn = Integer.parseInt(maxItems);
        }

        if (resultSetManagement != null) {
            Element max = resultSetManagement.element("max");
            if (max != null) {
                maxItemsToReturn = Integer.parseInt(max.getTextTrim());
            }
            Element after = resultSetManagement.element("after");
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
                    logger.error(e);
                }
            }
        }

        Element items = pubsub.addElement("items");
        items.addAttribute("node", node);

        entry = null;
        int totalEntriesCount = getNodeItems(items, maxItemsToReturn, afterItemId);

        if ((false == Configuration.getInstance().isLocalNode(node))
                && (0 == rsmEntriesCount)) {
            logger.debug("No results in cache for remote node, so "
                    + "we're going federated to get more");
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
            Element rsm = pubsub.addElement("set", "http://jabber.org/protocol/rsm");

            if (firstItem != null) {
                rsm.addElement("first").setText(firstItem);
                rsm.addElement("last").setText(lastItem);
            }
            rsm.addElement("count").setText(Integer.toString(totalEntriesCount));
        }

        reply.setChildElement(pubsub);
    }

    private boolean userCanViewNode() throws NodeStoreException {
        NodeMembership nodeMembership = channelManager.getNodeMembership(node, actor);

        if (getNodeViewAcl().canViewNode(node, nodeMembership,
                getNodeAccessModel(), Configuration.getInstance().isLocalJID(actor))) {
            return true;
        }
        NodeAclRefuseReason reason = getNodeViewAcl().getReason();
        createExtendedErrorReply(reason.getType(), reason.getCondition(),
                reason.getAdditionalErrorElement());
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
    private int getNodeItems(Element items, int maxItemsToReturn, String afterItemId) throws NodeStoreException {

        CloseableIterator<NodeItem> itemIt = channelManager.getNodeItems(node, afterItemId, maxItemsToReturn);
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
            logger.debug("Including RSM there are " + rsmEntriesCount + " items for node " + node);
            return channelManager.countNodeItems(node);
        } finally {
            if (itemIt != null) {
                itemIt.close();
            }
        }
    }

    private void addItemToResponse(NodeItem nodeItem, Element parent) {
        try {
            entry = xmlReader.read(new StringReader(nodeItem.getPayload()))
                    .getRootElement();
            Element item = parent.addElement("item");
            item.addAttribute("id", nodeItem.getId());
            item.add(entry);
        } catch (DocumentException e) {
            logger.error("Error parsing a node entry, ignoring. " + nodeItem);
        }
    }

    private void createExtendedErrorReply(Type type, Condition condition, String additionalElement) {
        reply.setType(IQ.Type.error);
        Element standardError = new DOMElement(condition.toString(), new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
        Element extraError = new DOMElement(additionalElement, new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
        Element error = new DOMElement("error");
        error.addAttribute("type", type.toString());
        error.add(standardError);
        error.add(extraError);
        reply.setChildElement(error);
    }

    public boolean accept(Element elm) {
        return elm.getName().equals("items");
    }
}
