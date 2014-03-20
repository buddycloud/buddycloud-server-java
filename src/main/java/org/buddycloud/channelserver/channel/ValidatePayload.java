package org.buddycloud.channelserver.channel;

import org.buddycloud.channelserver.channel.node.configuration.field.ContentType;
import org.buddycloud.channelserver.channel.validate.AtomEntry;
import org.buddycloud.channelserver.channel.validate.UnknownContentTypeException;
import org.buddycloud.channelserver.channel.validate.ValidateEntry;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.utils.node.item.payload.Atom;

public class ValidatePayload {
	
	public static final String UNSUPPORTED_CONTENT_TYPE = "unsupported-content-type";
	private ChannelManager channelManager;
	private String node;

	public ValidatePayload(ChannelManager channelManager, String node) {
		this.channelManager = channelManager;
		this.node = node;
	}

	public ValidateEntry getValidator() throws NodeStoreException, UnknownContentTypeException {
		String contentType = channelManager.getNodeConfValue(node, ContentType.FIELD_NAME);
		
		if ((null == contentType) || (contentType.equals(Atom.NS))) {
			return new AtomEntry();
		}
		throw new UnknownContentTypeException("Unknown node content type " + contentType);
	}
}