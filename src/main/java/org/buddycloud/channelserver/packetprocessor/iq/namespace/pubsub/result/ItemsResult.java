package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class ItemsResult extends PubSubElementProcessorAbstract {

	private static final String MISSING_NODE = "Missing node";
	private static final String MISSING_ITEMS_ELEMENT = "Missing items element";
	private String node;
	
	public ItemsResult(ChannelManager channelManager) {
		this.channelManager = channelManager;
	}

	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
        node = elm.attributeValue("node");
        if ((null == node) || (true == node.equals(""))) {
        	throw new NullPointerException(MISSING_NODE);
        }
        Element items = elm.element("items");
		if (items == null) {
			throw new NullPointerException(MISSING_ITEMS_ELEMENT);
		}
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}
}