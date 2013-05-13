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

		if (node == null) {
			getUserAffiliations(affiliations);
		} else {
			getNodeAffiliations(affiliations);
		}
	}

	private void getNodeAffiliations(Element affiliations)
			throws NodeStoreException, InterruptedException {
		if (false == channelManager.isLocalNode(node)
				&& (false == channelManager.isCachedNode(node))) {
			makeRemoteRequest(node.split("/")[2]);
			return;
		}
		ResultSet<NodeAffiliation> nodeAffiliations = channelManager
				.getNodeAffiliations(node);

		if ((null != requestIq.getElement().element("pubsub").element("set"))
				&& (0 == nodeAffiliations.size())
				&& (false == channelManager.isLocalNode(node))) {
			makeRemoteRequest(node);
			return;
		}
		
		for (NodeAffiliation nodeAffiliation : nodeAffiliations) {

			logger.trace("Adding affiliation for " + nodeAffiliation.getUser()
					+ " affiliation " + nodeAffiliation.getAffiliation());
			affiliations
					.addElement("affiliation")
					.addAttribute("node", nodeAffiliation.getNodeId())
					.addAttribute("affiliation",
							nodeAffiliation.getAffiliation().toString())
					.addAttribute("jid", nodeAffiliation.getUser().toString());
		}
		outQueue.put(result);
	}

	private void getUserAffiliations(Element affiliations)
			throws NodeStoreException, InterruptedException {
		if (false == channelManager.isLocalJID(actorJid)
				&& (false == channelManager.isCachedJID(requestIq.getFrom()))) {
			makeRemoteRequest(actorJid.getDomain());
			return;
		}
		ResultSet<NodeAffiliation> affs = channelManager
				.getUserAffiliations(actorJid);

		if ((null != requestIq.getElement().element("pubsub").element("set"))
				&& (0 == affs.size())
				&& (false == channelManager.isLocalJID(actorJid))) {
			makeRemoteRequest(actorJid.getDomain());
			return;
		}
		
		for (NodeAffiliation aff : affs) {
			logger.trace("Adding affiliation for " + aff.getUser()
					+ " affiliation " + aff.getAffiliation()
					+ " (no node provided)");
			affiliations
					.addElement("affiliation")
					.addAttribute("node", aff.getNodeId())
					.addAttribute("affiliation",
							aff.getAffiliation().toString())
					.addAttribute("jid", aff.getUser().toString());
		}
		outQueue.put(result);
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