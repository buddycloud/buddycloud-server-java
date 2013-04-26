package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.dom4j.Element;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class AffiliationProcessor extends AbstractMessageProcessor  {
 
	private JID jid;
	private Affiliations affiliation;

	private static final Logger logger = Logger
			.getLogger(AffiliationProcessor.class);

	public AffiliationProcessor(BlockingQueue<Packet> outQueue,
			Properties configuration, ChannelManager channelManager) {
		super(channelManager, configuration, outQueue);
	}

	@Override
	public void process(Message packet) throws Exception {
		message = packet;

		handleAffiliationElement();

		if (false == channelManager.isLocalNode(node)) {
			sendLocalNotifications();
		}
	}

	private void handleAffiliationElement() throws NodeStoreException {
		Element affiliationElement = message.getElement().element("event")
				.element("affiliation");
		if (null == affiliationElement) {
			return;
		}
		jid = new JID(affiliationElement.attributeValue("jid"));
		node = affiliationElement.attributeValue("node");
		affiliation = Affiliations.valueOf(affiliationElement
				.attributeValue("affiliation"));
		if (true == channelManager.isLocalNode(node)) {
			return;
		}
        storeNewAffiliation();
	}

	private void storeNewAffiliation() throws NodeStoreException {
		addRemoteNode();
		channelManager.setUserAffiliation(node, jid, affiliation);
	}

	private void addRemoteNode() {
        try { 
            channelManager.addRemoteNode(node); 
        } catch (NodeStoreException e) { 
        	logger.error(e);
        }
	}
}