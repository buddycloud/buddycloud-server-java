package org.buddycloud.channelserver.pubsub.model.impl;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.xmpp.packet.JID;

/**
 * Basic implementation of {@link GlobalItemID}
 */
public class GlobalItemIDImpl implements GlobalItemID {

	private static final Pattern STRING_PATTERN = Pattern.compile("^tag:([^,]+),([^,]+),([^,]+)$");
	
	/**
	 * Creates a new global item ID
	 * @param service the JID of the pubsub service hosting the node
	 * @param nodeID the id of the node containing the item
	 * @param itemID the id of the item
	 */
	public GlobalItemIDImpl(JID service, String nodeID, String itemID) {
		this.service = service;
		this.nodeID = nodeID;
		this.itemID = itemID;
	}

	private JID service;
	private String nodeID;
	private String itemID;
	
	@Override
	public JID getService() {
		return service;
	}
	
	@Override
	public String getNodeID() {
		return nodeID;
	}
	
	@Override
	public String getItemID() {
		return itemID;
	}
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		
		builder.append("tag:");
		builder.append(service.toString());
		builder.append(",");
		builder.append(nodeID);
		builder.append(",");
		builder.append(itemID);
		
		return builder.toString();
	}
	
	/**
	 * Creates a new {@link GlobalItemID} from a tag string which looks a bit like:
	 * <blockquote><code>tag:pubsub.server.com,a/node/id,an/item/id</code></blockquote>
	 * @param str The tag string
	 * @return the {@link GlobalItemID}
	 */
	public static GlobalItemID fromString(final String str) {
		Matcher matcher = STRING_PATTERN.matcher(str);
		
		if(!matcher.matches()) {
			throw new IllegalArgumentException(str + " is not a valid GlobalItemID String. Expected something like 'tag:example.com,node,item'.");
		}
		
		JID service = new JID(matcher.group(1));
		
		// Handle an issue with certain clients
		if((service.getNode() != null) && service.getNode().equals("null")) {
			service = new JID(null, service.getDomain(), service.getResource());
		}
		
		return new GlobalItemIDImpl(service, matcher.group(2), matcher.group(3));
	}
}
