package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class AffiliationsGet implements PubSubElementProcessor {

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;

	private static final Logger LOGGER = Logger
			.getLogger(AffiliationsGet.class);

	public AffiliationsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		IQ result = IQ.createResultIQ(reqIQ);
		Element pubsub = result.setChildElement(PubSubGet.ELEMENT_NAME,
				JabberPubsub.NS_PUBSUB_OWNER);
		Element affiliations = pubsub.addElement("affiliations");

		String node = elm.attributeValue("node");

		if (actorJID == null) {
			actorJID = reqIQ.getFrom();
		}

		if (node == null) {
			Collection<NodeAffiliation> affs = channelManager
					.getUserAffiliations(actorJID);

			for (NodeAffiliation aff : affs) {
				LOGGER.trace("Adding affiliation for "
						+ aff.getUser() + " affiliation "
						+ aff.getAffiliation() + " (no node provided)");
				affiliations
						.addElement("affiliation")
						.addAttribute("node", aff.getNodeId())
						.addAttribute("affiliation",
								aff.getAffiliation().toString())
						.addAttribute("jid", aff.getUser().toString());
			}
		} else {
			Collection<NodeAffiliation> nodeAffiliations = channelManager
					.getNodeAffiliations(node);

			for (NodeAffiliation nodeAffiliation : nodeAffiliations) {

				LOGGER.trace("Adding affiliation for "
						+ nodeAffiliation.getUser() + " affiliation "
						+ nodeAffiliation.getAffiliation());
				affiliations
						.addElement("affiliation")
						.addAttribute("node", nodeAffiliation.getNodeId())
						.addAttribute("affiliation",
								nodeAffiliation.getAffiliation().toString())
						.addAttribute("jid",
								nodeAffiliation.getUser().toString());
			}
		}
		outQueue.put(result);
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("affiliations");
	}
}
