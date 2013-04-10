package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.time.DateFormatUtils;
import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ValidateEntry;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set.SubscribeSet;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class ItemsResult extends PubSubElementProcessorAbstract {

	private static final String MISSING_NODE = "Missing node";
    private static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
 
	private static final Logger logger = Logger.getLogger(ItemsResult.class);
	private String node;
	private SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
	
	public ItemsResult(ChannelManager channelManager) {
		this.channelManager = channelManager;
	}

	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {

		this.request = reqIQ;

		node = elm.attributeValue("node");

		if ((null == node) || (true == node.equals(""))) {
			throw new NullPointerException(MISSING_NODE);
		}

		List<Element> items = request.getElement()
				.element("pubsub")
				.element("items")
				.elements("item");

		for (Element item : items) {
			processItem(item);
		}
	}

	private void processItem(Element item) throws ParseException, NodeStoreException {

		Element entry = item.element("entry");
		
		try {
		    Date updatedDate = sdf.parse(entry.elementText("updated"));
			NodeItemImpl nodeItem = new NodeItemImpl(node, entry.elementText("id"),
					updatedDate, entry.asXML());
			channelManager.addNodeItem(nodeItem);
		} catch (ParseException e) {
			logger.error(e);
			return;
		}
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}
}