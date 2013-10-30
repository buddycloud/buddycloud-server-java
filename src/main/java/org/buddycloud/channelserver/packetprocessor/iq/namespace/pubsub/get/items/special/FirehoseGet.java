package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.special;

import java.io.StringReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class FirehoseGet extends PubSubElementProcessorAbstract {

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

	private boolean isAdmin = false;

	private static final Logger logger = Logger.getLogger(FirehoseGet.class);

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

		if (false == channelManager.isLocalJID(request.getFrom()))
			response.getElement().addAttribute("remote-server-discover",
					"false");

		pubsub = response.getElement().addElement("pubsub",
				JabberPubsub.NAMESPACE_URI);
		try {
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

	private void determineAdminUserStatus() {
		for (JID user : getAdminUsers()) {
			if (true == user.toBareJID().equals(actor.toBareJID())) {
				isAdmin = true;
				return;
			}
		}
	}

	private void parseRsmElement() {
		Element rsmElement = pubsub.element("set");
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
				String.valueOf(channelManager.getFirehoseItemCount(isAdmin)));
	}

	private void addItems() throws NodeStoreException {
		if (-1 == maxResults) maxResults = 50;
		CloseableIterator<NodeItem> items = channelManager.getFirehose(maxResults, afterItemId, isAdmin);
		String lastNode = "";
		NodeItem item;
		Element itemsElement = null;
		Element itemElement;
		Element entry;
		while (items.hasNext()) {
			item = items.next();

			if (false == item.getNodeId().equals(lastNode)) {
				itemsElement = pubsub.addElement("items");
				itemsElement.addAttribute("node", item.getNodeId());
				lastNode = item.getNodeId();
			}
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

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}
}