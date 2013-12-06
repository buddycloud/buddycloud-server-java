package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.io.StringReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class ThreadGet extends PubSubElementProcessorAbstract {

	private Element resultSetManagement;
	private String firstItem;
	private String lastItem;
	private int totalEntriesCount;

	private Date maxAge;
	private Integer maxItems;

	private Element pubsub;
	private SAXReader xmlReader;
	private String nodeEnding = "/posts";

	// RSM details
	private String firstItemId = null;
	private String lastItemId = null;
	private String afterItemId = null;
	private int maxResults = -1;
	private String parentId;
	private NodeViewAcl nodeViewAcl;
	private Map<String, String> nodeConfiguration;
	private NodeItem parentItem;

	private static final Logger logger = Logger.getLogger(RecentItemsGet.class);

	public static final String NS_RSM = "http://jabber.org/protocol/rsm";

	public ThreadGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		setChannelManager(channelManager);
		setOutQueue(outQueue);

		xmlReader = new SAXReader();
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		response = IQ.createResultIQ(reqIQ);
		request = reqIQ;
		actor = actorJID;
		resultSetManagement = rsm;

		if (null == actor) actor = request.getFrom();

		if (false == isValidStanza()) {
			outQueue.put(response);
			return;
		}

		try {
			if (false == channelManager.isLocalJID(request.getFrom())) {
				response.getElement().addAttribute("remote-server-discover",
						"false");
			}
			pubsub = response.getElement().addElement("pubsub",
					JabberPubsub.NAMESPACE_URI);
			if ((false == userCanViewNode()) || (false == itemExists())) {
				outQueue.put(response);
				return;
			}
			parseRsmElement();
			addItems();
			addRsmElement();
			outQueue.put(response);
		} catch (NodeStoreException e) {
			logger.error(e);
			response.getElement().remove(pubsub);
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
		}
		outQueue.put(response);
	}

	private boolean itemExists() throws NodeStoreException {
		if (null != (parentItem = channelManager.getNodeItem(node, parentId))) return true;
		createExtendedErrorReply(PacketError.Type.modify,
				PacketError.Condition.bad_request, "parent-item-not-found");
		return false;
	}

	private void parseRsmElement() {
		Element rsmElement = request.getChildElement().element("set");
		if (null == rsmElement)
			return;
		Element max;
		Element after;
		if (null != (max = rsmElement.element("max")))
			maxResults = Integer.parseInt(max.getTextTrim());
		if (null != (after = rsmElement.element("after")))
			afterItemId = after.getTextTrim();
	}

	private void addRsmElement() throws NodeStoreException {
		if (null == firstItemId) return;
		Element rsm = pubsub.addElement("set");
		rsm.addNamespace("", NS_RSM);
		rsm.addElement("first").setText(firstItemId);
		rsm.addElement("last").setText(lastItemId);
		rsm.addElement("count").setText(
				String.valueOf(channelManager.getCountNodeThread(node, parentId)));
	}

	private void addItems() throws NodeStoreException {

		CloseableIterator<NodeItem> items = channelManager.getNodeItemThread(node, parentId, afterItemId, maxResults);
		NodeItem item;
		Element entry;
		Element itemElement;
		Element itemsElement = pubsub.addElement("items");
		itemsElement.addAttribute("node", node);
		
		while (items.hasNext()) {
			item = items.next();

			try {
				entry = xmlReader.read(new StringReader(item.getPayload()))
						.getRootElement();
				itemElement = itemsElement.addElement("item");
				itemElement.addAttribute("id", item.getId());
				if (null == firstItemId)
					firstItemId = item.getId();
				lastItemId = item.getId();
				itemElement.add(entry);
			} catch (DocumentException e) {
				logger.error("Error parsing a node entry, ignoring. "
						+ item.getId());
			}
		}
	}

	private boolean isValidStanza() {
		Element thread = request.getChildElement().element("thread");
		try {
			node = thread.attributeValue("node");
			if (null == node) {
				createExtendedErrorReply(PacketError.Type.modify,
						PacketError.Condition.bad_request, "nodeid-required");
				return false;
			}
			parentId = thread.attributeValue("item_id");
			if (null == parentId) {
				createExtendedErrorReply(PacketError.Type.modify,
						PacketError.Condition.bad_request, "itemid-required");
				return false;
			}
			if (false == channelManager.nodeExists(node)) {
				setErrorCondition(PacketError.Type.cancel,
						PacketError.Condition.item_not_found);
				return false;
			}
			nodeConfiguration = channelManager.getNodeConf(node);
		} catch (NullPointerException e) {
			logger.error(e);
			setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
			return false;
		} catch (NodeStoreException e) {
			logger.error(e);
			setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
			return false;
		}
		return true;
	}
	
	private boolean userCanViewNode() throws NodeStoreException {
		NodeSubscription nodeSubscription = channelManager.getUserSubscription(
				node, actor);
		NodeAffiliation nodeAffiliation = channelManager.getUserAffiliation(
				node, actor);

		Affiliations possibleExistingAffiliation = Affiliations.none;
		Subscriptions possibleExistingSubscription = Subscriptions.none;
		if (null != nodeSubscription) {
			if (null != nodeAffiliation.getAffiliation()) {
				possibleExistingAffiliation = nodeAffiliation.getAffiliation();
			}
			if (null != nodeSubscription.getSubscription()) {
				possibleExistingSubscription = nodeSubscription
						.getSubscription();
			}
		}
		if (true == getNodeViewAcl().canViewNode(node,
				possibleExistingAffiliation, possibleExistingSubscription,
				getNodeAccessModel(), channelManager.isLocalJID(actor))) {
			return true;
		}
		NodeAclRefuseReason reason = getNodeViewAcl().getReason();
		createExtendedErrorReply(reason.getType(), reason.getCondition(),
				reason.getAdditionalErrorElement());
		return false;
	}
	
	private AccessModels getNodeAccessModel() {
		if (false == nodeConfiguration.containsKey(AccessModel.FIELD_NAME)) {
			return AccessModels.authorize;
		}
		return AccessModels.createFromString(nodeConfiguration
				.get(AccessModel.FIELD_NAME));
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
	public boolean accept(Element elm) {
		return elm.getName().equals("thread");
	}
}