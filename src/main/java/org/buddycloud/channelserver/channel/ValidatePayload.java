package org.buddycloud.channelserver.channel;

import java.io.File;
import java.util.Collection;

import net.xeoh.plugins.base.PluginManager;
import net.xeoh.plugins.base.impl.PluginManagerFactory;
import net.xeoh.plugins.base.util.PluginManagerUtil;

import org.buddycloud.channelserver.channel.node.configuration.field.ContentType;
import org.buddycloud.channelserver.channel.validate.AtomEntry;
import org.buddycloud.channelserver.channel.validate.UnknownContentTypeException;
import org.buddycloud.channelserver.channel.validate.PayloadValidator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;

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

        PluginManager pm = PluginManagerFactory.createPluginManager();
        pm.addPluginsFrom(new File("plugins/").toURI());

        Collection<PayloadValidator> validators = new PluginManagerUtil(pm).getPlugins(PayloadValidator.class);
        for ( PayloadValidator validator : validators ) {
            if (validator.canValidate(contentType)) {
                return validator;
            }
        }

		throw new UnknownContentTypeException("Unknown node content type " + contentType);
	}
}