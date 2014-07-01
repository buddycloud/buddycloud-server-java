package org.buddycloud.channelserver.channel;

import java.io.File;

import net.xeoh.plugins.base.PluginManager;
import net.xeoh.plugins.base.impl.PluginManagerFactory;
import net.xeoh.plugins.base.options.getplugin.OptionCapabilities;

import org.buddycloud.channelserver.channel.node.configuration.field.ContentType;
import org.buddycloud.channelserver.channel.validate.AtomEntry;
import org.buddycloud.channelserver.channel.validate.UnknownContentTypeException;
import org.buddycloud.channelserver.channel.validate.PayloadValidator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;

public class ValidatePayload {
	
	public static final String UNSUPPORTED_CONTENT_TYPE = "unsupported-content-type";
	private ChannelManager channelManager;
	private String node;
    private PluginManager pluginManager;

	public ValidatePayload(ChannelManager channelManager, String node) {
		this.channelManager = channelManager;
		this.node = node;
	}

	public PayloadValidator getValidator() throws NodeStoreException, UnknownContentTypeException {
		String contentType = channelManager.getNodeConfValue(node, ContentType.FIELD_NAME);

		AtomEntry defaultValidator = new AtomEntry();
		if ((null == contentType) || (defaultValidator.canValidate(contentType))) {
			return defaultValidator;
		}

		PayloadValidator validator = getValidatorFromPlugins(contentType);

		if (null == validator) {
		    throw new UnknownContentTypeException("Unknown node content type " + contentType);
		}

		return validator;
	}

    private PayloadValidator getValidatorFromPlugins(String contentType) {
        return getPluginManager().getPlugin(
		        PayloadValidator.class, new OptionCapabilities(contentType));
    }

    public void setPluginManager(PluginManager pluginManager) {
        this.pluginManager = pluginManager;
    }

    private PluginManager getPluginManager() {
        if (null == pluginManager) {
            setDefaultPluginManager();
        }

        return pluginManager;
    }

    private void setDefaultPluginManager() {
        PluginManager pluginManager = PluginManagerFactory.createPluginManager();
        pluginManager.addPluginsFrom(new File("plugins/").toURI());
        setPluginManager(pluginManager);
    }
}