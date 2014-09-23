package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.special;

import java.io.StringReader;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class FirehoseGet extends PubSubElementProcessorAbstract {

	private static final int DEF_MAX_RESULTS = 50;
	private static final Logger LOGGER = Logger.getLogger(FirehoseGet.class);
	
	private Element pubsub;
	private SAXReader xmlReader;
	private boolean isAdmin = false;

	// RSM details
	private String firstItemId = null;
	private String lastItemId = null;
	private String afterItemId = null;
	private int maxResults = -1;

	public FirehoseGet(BlockingQueue<Packet> outQueue,
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
		node = elm.attributeValue("node");
		resultSetManagement = rsm;

		if (null == actor) {
			actor = request.getFrom();
		}
		determineAdminUserStatus();

		if (false == channelManager.isLocalJID(request.getFrom())) {
			response.getElement().addAttribute("remote-server-discover",
					"false");
		}

		pubsub = response.getElement().addElement("pubsub",
				JabberPubsub.NAMESPACE_URI);
		try {
			parseRsmElement();
			addItems();
			addRsmElement();
			outQueue.put(response);
		} catch (NodeStoreException e) {
			LOGGER.error(e);
			response.getElement().remove(pubsub);
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
		}
		outQueue.put(response);

	}

	private void determineAdminUserStatus() {
		for (JID user : getAdminUsers()) {
			if (user.toBareJID().equals(actor.toBareJID())) {
				isAdmin = true;
				return;
			}
		}
	}

	private void parseRsmElement() {
		if (null == resultSetManagement) {
			return;
		}
		Element max = resultSetManagement.element("max");
		if (max != null) {
			maxResults = Integer.parseInt(max.getTextTrim());
		}
		Element after = resultSetManagement.element("after");
		if (after != null) {
			afterItemId = after.getTextTrim();
		}
	}

	private void addRsmElement() throws NodeStoreException {
		if (firstItemId == null) {
			return;
		}
		Element rsm = pubsub.addElement("set");
		rsm.addNamespace("", NS_RSM);
		rsm.addElement("first").setText(firstItemId);
		rsm.addElement("last").setText(lastItemId);
		rsm.addElement("count").setText(
				String.valueOf(channelManager.getFirehoseItemCount(isAdmin)));
	}

	private void addItems() throws NodeStoreException {
		if (-1 == maxResults) {
			maxResults = DEF_MAX_RESULTS;
		}
		CloseableIterator<NodeItem> items = channelManager.getFirehose(maxResults, afterItemId, isAdmin);
		String lastNode = "";
		Element itemsElement = null;
		while (items.hasNext()) {
			NodeItem item = items.next();
			if (false == item.getNodeId().equals(lastNode)) {
				itemsElement = pubsub.addElement("items");
				itemsElement.addAttribute("node", item.getNodeId());
				lastNode = item.getNodeId();
			}
			try {
				Element entry = xmlReader.read(new StringReader(item.getPayload()))
						.getRootElement();
				Element itemElement = itemsElement.addElement("item");
				itemElement.addAttribute("id", item.getId());
				if (null == firstItemId) {
					firstItemId = item.getId();
				}
				lastItemId = item.getId();
				itemElement.add(entry);
			} catch (DocumentException e) {
				LOGGER.error("Error parsing a node entry, ignoring. "
						+ item.getId());
			}
		}
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}
}