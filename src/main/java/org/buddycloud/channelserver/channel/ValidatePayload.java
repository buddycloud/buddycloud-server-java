package org.buddycloud.channelserver.channel;

import org.buddycloud.channelserver.channel.node.configuration.field.ContentType;
import org.buddycloud.channelserver.channel.validate.AtomEntry;
import org.buddycloud.channelserver.channel.validate.IodefDocument;
import org.buddycloud.channelserver.channel.validate.UnknownContentTypeException;
import org.buddycloud.channelserver.channel.validate.PayloadValidator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.utils.node.item.payload.Iodef;

public class ValidatePayload {
	
	public static final String UNSUPPORTED_CONTENT_TYPE = "unsupported-content-type";
	private ChannelManager channelManager;
	private String node;

	public ValidatePayload(ChannelManager channelManager, String node) {
		this.channelManager = channelManager;
		this.node = node;
	}

	public PayloadValidator getValidator() throws NodeStoreException, UnknownContentTypeException {
		String contentType = channelManager.getNodeConfValue(node, ContentType.FIELD_NAME);

		PayloadValidator defaultValidator = new AtomEntry();
		if ((null == contentType) || (defaultValidator.canValidate(contentType))) {
			return defaultValidator;
		}

		if (contentType.equals(Iodef.NS)) {
			return new IodefDocument();
		}
		throw new UnknownContentTypeException("Unknown node content type " + contentType);
	}
}