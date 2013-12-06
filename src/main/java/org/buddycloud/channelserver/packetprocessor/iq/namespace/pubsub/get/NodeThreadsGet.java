package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.io.StringReader;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.NodeThread;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
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

public class NodeThreadsGet extends PubSubElementProcessorAbstract {

	private static final int MAX_THREADS_TO_RETURN = 15;
	private int max;
	private String afterId;

	public NodeThreadsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		setChannelManager(channelManager);
		setOutQueue(outQueue);
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		this.request = reqIQ;
		this.node = elm.attributeValue("node");
		this.actor = actorJID;
		this.resultSetManagement = rsm;
		this.max = MAX_THREADS_TO_RETURN;

		if (actor == null) {
			actor = request.getFrom();
		}
		if (!isValidStanza()) {
			outQueue.put(response);
			return;
		}
		if (!userCanViewNode()) {
			outQueue.put(response);
			return;
		}
		if (!parseRsmElement()) {
			outQueue.put(response);
			return;
		}
		getNodeThreads();
		addRsmElement();
		outQueue.put(response);
	}

	private void addRsmElement() throws NodeStoreException {
		if (firstItem == null) {
			return;
		}
		Element pubsubEl = response.getElement().element("pubsub");
		Element rsm = pubsubEl.addElement("set", NS_RSM);
		rsm.addElement("first", NS_RSM).setText(firstItem);
		rsm.addElement("last", NS_RSM).setText(lastItem);

		Integer nodeThreadCount = channelManager.countNodeThreads(node);
		rsm.addElement("count", NS_RSM).setText(nodeThreadCount.toString());
	}

	private void getNodeThreads() throws NodeStoreException, DocumentException {
		ResultSet<NodeThread> nodeThreads = channelManager.getNodeThreads(node,
				afterId, max);
		this.response = IQ.createResultIQ(request);
		Element pubsubEl = response.getElement().addElement("pubsub",
				JabberPubsub.NAMESPACE_URI);
		SAXReader xmlReader = new SAXReader();
		for (NodeThread nodeThread : nodeThreads) {
			Element threadEl = pubsubEl.addElement("thread");
			threadEl.addAttribute("node", node);
			threadEl.addAttribute("id", nodeThread.getId());
			threadEl.addAttribute("updated",
					Conf.formatDate(nodeThread.getUpdated()));
			ResultSet<NodeItem> items = nodeThread.getItems();
			for (NodeItem item : items) {
				Element entry = xmlReader.read(
						new StringReader(item.getPayload())).getRootElement();
				Element itemElement = threadEl.addElement("item");
				itemElement.addAttribute("id", item.getId());
				itemElement.add(entry);
			}
		}
		if (!nodeThreads.isEmpty()) {
			this.firstItem = nodeThreads.getFirst(1).iterator().next().getId();
			this.lastItem = nodeThreads.getLast(1).iterator().next().getId();
		}
	}

	private boolean isValidStanza() throws NodeStoreException {
		if (node == null) {
			createExtendedErrorReply(PacketError.Type.modify,
					PacketError.Condition.bad_request, "nodeid-required");
			return false;
		}
		if (!channelManager.nodeExists(node)) {
			setErrorCondition(PacketError.Type.cancel,
					PacketError.Condition.item_not_found);
			return false;
		}
		return true;
	}

	private AccessModels getNodeAccessModel(
			Map<String, String> nodeConfiguration) {
		if (!nodeConfiguration.containsKey(AccessModel.FIELD_NAME)) {
			return AccessModels.authorize;
		}
		return AccessModels.createFromString(nodeConfiguration
				.get(AccessModel.FIELD_NAME));
	}

	private boolean userCanViewNode() throws NodeStoreException {
		NodeSubscription nodeSubscription = channelManager.getUserSubscription(
				node, actor);
		NodeAffiliation nodeAffiliation = channelManager.getUserAffiliation(
				node, actor);

		Affiliations affiliation = Affiliations.none;
		Subscriptions subscription = Subscriptions.none;
		if (nodeSubscription != null) {
			affiliation = nodeAffiliation.getAffiliation();
			subscription = nodeSubscription.getSubscription();
		}
		NodeViewAcl nodeViewAcl = new NodeViewAcl();
		Map<String, String> nodeConfiguration = channelManager
				.getNodeConf(node);

		if (nodeViewAcl.canViewNode(node, affiliation, subscription,
				getNodeAccessModel(nodeConfiguration),
				channelManager.isLocalJID(actor))) {
			return true;
		}

		NodeAclRefuseReason reason = nodeViewAcl.getReason();
		createExtendedErrorReply(reason.getType(), reason.getCondition(),
				reason.getAdditionalErrorElement());
		return false;
	}

	private boolean parseRsmElement() throws NodeStoreException {
		if (resultSetManagement == null) {
			return true;
		}
		Element maxEl = resultSetManagement.element("max");
		if (maxEl != null) {
			this.max = Integer.parseInt(maxEl.getTextTrim());
		}
		Element afterEl = resultSetManagement.element("after");
		if (afterEl != null) {
			this.afterId = afterEl.getTextTrim();
			if (channelManager.getNodeItem(node, afterId) == null) {
				setErrorCondition(PacketError.Type.cancel,
						PacketError.Condition.item_not_found);
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("threads");
	}
}
