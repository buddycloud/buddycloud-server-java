package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

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
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class RecentItemsGet extends PubSubElementProcessorAbstract {

	private SimpleDateFormat sdf;

	private Date maxAge;
	private Integer maxItems;

	private Element pubsub;
	private SAXReader xmlReader;
	private String nodeEnding = "/posts";

	// RSM details
	private String firstItemId = null;
	private String lastItemId = null;
	private GlobalItemID afterItemId = null;
	private int maxResults = -1;

	private static final Logger logger = Logger.getLogger(RecentItemsGet.class);

	public RecentItemsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		setChannelManager(channelManager);
		setOutQueue(outQueue);

		xmlReader = new SAXReader();

		sdf = new SimpleDateFormat(Conf.DATE_FORMAT);
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
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

		if (false == isValidStanza()) {
			outQueue.put(response);
			return;
		}

		if (false == channelManager.isLocalJID(request.getFrom())) {
			response.getElement().addAttribute("remote-server-discover",
					"false");
		}
		pubsub = response.getElement().addElement("pubsub",
				JabberPubsub.NAMESPACE_URI);
		try {
			parseRsmElement();
			addRecentItems();
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

	private void parseRsmElement() {
		if (null == resultSetManagement)
			return;
		Element max;
		Element after;
		if (null != (max = resultSetManagement.element("max")))
			maxResults = Integer.parseInt(max.getTextTrim());
		if (null != (after = resultSetManagement.element("after"))) {
			try {
				afterItemId = GlobalItemIDImpl.fromString(after.getTextTrim());
			} catch(IllegalArgumentException e) {
				logger.error(e);
				createExtendedErrorReply(Type.modify, Condition.bad_request, "Could not parse the 'after' id: " + after);
				return;
			}
		}
	}

	private void addRsmElement() throws NodeStoreException {
		if (null == firstItemId) return;
		Element rsm = pubsub.addElement("set");
		rsm.addNamespace("", NS_RSM);
		rsm.addElement("first").setText(firstItemId);
		rsm.addElement("last").setText(lastItemId);
		rsm.addElement("count").setText(
				String.valueOf(channelManager.getCountRecentItems(actor,
						maxAge, maxItems, nodeEnding)));
	}

	private void addRecentItems() throws NodeStoreException {
		CloseableIterator<NodeItem> items = channelManager.getRecentItems(
				actor, maxAge, maxItems, maxResults, afterItemId, nodeEnding);
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
				
				Element entryIdEl = entry.element("id");
				
				String itemId = item.getId();
				
				if(entryIdEl != null) {
					itemId = entryIdEl.getTextTrim();
				}
				
				itemElement = itemsElement.addElement("item");
				itemElement.addAttribute("id", item.getId());
				
				if (null == firstItemId)
					firstItemId = itemId;
				lastItemId = itemId;
				itemElement.add(entry);
			} catch (DocumentException e) {
				logger.error("Error parsing a node entry, ignoring. "
						+ item.getId());
			}
		}
	}

	private boolean isValidStanza() {
		Element recentItems = request.getChildElement().element("recent-items");
		try {
			String max = recentItems.attributeValue("max");
			if (null == max) {
				createExtendedErrorReply(PacketError.Type.modify,
						PacketError.Condition.bad_request, "max-required");
				return false;
			}
			maxItems = Integer.parseInt(max);
			String since = recentItems.attributeValue("since");
			if (null == since) {
				createExtendedErrorReply(PacketError.Type.modify,
						PacketError.Condition.bad_request, "since-required");
				return false;
			}
			maxAge = sdf.parse(since);

		} catch (NumberFormatException e) {
			logger.error(e);
			createExtendedErrorReply(PacketError.Type.modify,
					PacketError.Condition.bad_request,
					"invalid-max-value-provided");
			return false;
		} catch (ParseException e) {
			createExtendedErrorReply(PacketError.Type.modify,
					PacketError.Condition.bad_request,
					"invalid-since-value-provided");
			logger.error(e);
			return false;
		}
		return true;
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("recent-items");
	}
}