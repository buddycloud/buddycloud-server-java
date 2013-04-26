package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.util.List;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class AffiliationsResult extends PubSubElementProcessorAbstract {

	private String node;
	private IQ request;
	private boolean ownerRequest;
	private String lastNode = "";

	private static final Logger logger = Logger
			.getLogger(AffiliationsResult.class);

	public AffiliationsResult(ChannelManager channelManager) {
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		this.request = reqIQ;

		if (-1 != request.getFrom().toString().indexOf("@")) {
			logger.debug("Ignoring result packet, only interested in stanzas "
					+ "from other buddycloud servers");
			return;
		}

		node         = elm.attributeValue("node");
		ownerRequest = ((null == node) || (true == node.equals("")));

		@SuppressWarnings("unchecked")
		List<Element> affiliations = request.getElement().element("pubsub")
				.element("affiliations").elements("affiliation");

		for (Element affiliation : affiliations) {
			addAffiliation(affiliation);
		}
	}

	private void addAffiliation(Element affiliation) throws NodeStoreException {

		if (true == ownerRequest) {
			node = affiliation.attributeValue("node");
		}

		if ((false == lastNode.equals(node))
				&& (false == channelManager.nodeExists(node)))
			channelManager.addRemoteNode(node);

		JID jid = new JID(affiliation.attributeValue("jid"));
		channelManager.setUserAffiliation(node, jid, Affiliations
				.createFromString(affiliation.attributeValue("affiliation")));
		lastNode = node;
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("affiliations");
	}
}