package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.resultsetmanagement.ResultSet;

public class AffiliationsGet implements PubSubElementProcessor {

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;

	private IQ requestIq;
	private String node;
	private JID actorJid;
	private IQ result;
	private Element resultSetManagement;
	private String firstItem;
	private String lastItem;
	private int totalEntriesCount;

	private static final Logger logger = Logger
			.getLogger(AffiliationsGet.class);

	public AffiliationsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		result = IQ.createResultIQ(reqIQ);
		requestIq = reqIQ;
		actorJid = actorJID;
		node = elm.attributeValue("node");
		resultSetManagement = rsm;

		if (false == channelManager.isLocalJID(requestIq.getFrom())) {
			result.getElement().addAttribute("remote-server-discover", "false");
		}
		String namespace = JabberPubsub.NS_PUBSUB_OWNER;
		if (node == null) {
			namespace = JabberPubsub.NAMESPACE_URI;
		}

		Element pubsub = result.setChildElement(PubSubGet.ELEMENT_NAME,
				namespace);
		Element affiliations = pubsub.addElement("affiliations");

		if (actorJid == null) {
			actorJid = requestIq.getFrom();
		}

		int maxItemsToReturn = MAX_ITEMS_TO_RETURN;
		String afterItemId   = null;

		String max_items = elm.attributeValue("max_items");
		if (max_items != null) {
			maxItemsToReturn = Integer.parseInt(max_items);
		}

		if (resultSetManagement != null) {
			Element max = resultSetManagement.element("max");
			if (max != null) {
				maxItemsToReturn = Integer.parseInt(max.getTextTrim());
			}
			Element after = resultSetManagement.element("after");
			if (after != null) {
				afterItemId = after.getTextTrim();
			}
		}
		boolean isProcessedLocally = true;
		if (node == null) {
			isProcessedLocally = getUserAffiliations(affiliations, maxItemsToReturn, afterItemId);
		} else {
			isProcessedLocally = getNodeAffiliations(affiliations, maxItemsToReturn, afterItemId);
		}
		if (false == isProcessedLocally) return;

		if ((resultSetManagement != null)
				|| (totalEntriesCount > maxItemsToReturn)) {
			/*
			 * TODO, add result set here as defined in 6.5.4 Returning Some
			 * Items <set xmlns='http://jabber.org/protocol/rsm'> <first
			 * index='0'>368866411b877c30064a5f62b917cffe</first>
			 * <last>4e30f35051b7b8b42abe083742187228</last> <count>19</count>
			 * </set>
			 */
			Element rsmElement = pubsub.addElement("set",
					"http://jabber.org/protocol/rsm");

			if (firstItem != null) {
				rsmElement.addElement("first").setText(firstItem);
				rsmElement.addElement("last").setText(lastItem);
			}
			rsmElement.addElement("count")
					.setText(Integer.toString(totalEntriesCount));
		}
			
		outQueue.put(result);
	}

	private boolean getNodeAffiliations(Element affiliations, int maxItemsToReturn, String afterItemId)
			throws NodeStoreException, InterruptedException {
		if (false == channelManager.isLocalNode(node)
				&& (false == channelManager.isCachedNode(node))) {
			makeRemoteRequest(node.split("/")[2]);
			return false;
		}
		ResultSet<NodeAffiliation> nodeAffiliations;
		if (null == afterItemId) {
			nodeAffiliations = channelManager.getNodeAffiliations(node);
		} else {
			nodeAffiliations = channelManager
				.getNodeAffiliations(node, afterItemId, maxItemsToReturn);
		}

		if ((0 == nodeAffiliations.size())
			&& (false == channelManager.isLocalNode(node))) {
			makeRemoteRequest(node.split("/")[2]);
			return false;
		}
		
		for (NodeAffiliation nodeAffiliation : nodeAffiliations) {

			logger.trace("Adding affiliation for " + nodeAffiliation.getUser()
					+ " affiliation " + nodeAffiliation.getAffiliation());
			
			if (null == firstItem) firstItem = nodeAffiliation.getUser().toString();
			lastItem = nodeAffiliation.getUser().toString();
			
			affiliations
					.addElement("affiliation")
					.addAttribute("node", nodeAffiliation.getNodeId())
					.addAttribute("affiliation",
							nodeAffiliation.getAffiliation().toString())
					.addAttribute("jid", nodeAffiliation.getUser().toString());
		}
		totalEntriesCount = channelManager.countNodeAffiliations(node);
		return true;
	}

	private boolean getUserAffiliations(Element affiliations, int maxItemsToReturn, String afterItemId)
			throws NodeStoreException, InterruptedException {
		
		if (false == channelManager.isLocalJID(actorJid)
				&& (false == channelManager.isCachedJID(requestIq.getFrom()))) {
			makeRemoteRequest(actorJid.getDomain());
			return false;
		}
		
		ResultSet<NodeAffiliation> affs = channelManager
				.getUserAffiliations(actorJid, afterItemId, maxItemsToReturn);

		if ((null != resultSetManagement)
				&& (0 == affs.size())
				&& (false == channelManager.isLocalJID(actorJid))) {
			makeRemoteRequest(actorJid.getDomain());
			return false;
		}
		
		for (NodeAffiliation aff : affs) {
			logger.trace("Adding affiliation for " + aff.getUser()
					+ " affiliation " + aff.getAffiliation()
					+ " (no node provided)");
			
			if (null == firstItem) firstItem = aff.getNodeId();
			lastItem = aff.getNodeId();
			
			affiliations
					.addElement("affiliation")
					.addAttribute("node", aff.getNodeId())
					.addAttribute("affiliation",
							aff.getAffiliation().toString())
					.addAttribute("jid", aff.getUser().toString());
		}
		
		totalEntriesCount = channelManager.countUserAffiliations(actorJid);
		return true;
	}

	private void makeRemoteRequest(String node) throws InterruptedException {
		requestIq.setTo(new JID(node).getDomain());
		Element actor = requestIq.getElement().element("pubsub")
				.addElement("actor", JabberPubsub.NS_BUDDYCLOUD);
		actor.addText(requestIq.getFrom().toBareJID());
		outQueue.put(requestIq);
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("affiliations");
	}
}