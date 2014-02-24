package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.util.HashMap;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class Configuration extends PubSubElementProcessorAbstract {

	private IQ request;
	Element configure;

	private static final Logger logger = Logger
			.getLogger(Configuration.class);

	public Configuration(ChannelManager channelManager) {
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		this.request = reqIQ;

		if (-1 != request.getFrom().toString().indexOf("@")) {
			logger.debug("Ignoring result packet, only interested in stanzas "
					+ "from other buddycloud servers");
			return;
		}
		
		configure = request.getChildElement().element("configure");
        node = configure.attributeValue("node");
        if (0 == node.length()) return;
        
        if (false == channelManager.nodeExists(node)) {
        	channelManager.addRemoteNode(node);
        }
        setNodeConfiguration();
	}
	
	private void setNodeConfiguration() throws Exception {
		try {
			getNodeConfigurationHelper().parse(request);
			updateNodeConfiguration(getNodeConfigurationHelper()
					.getValues());
		} catch (NodeConfigurationException e) {
			logger.error("Node configuration exception", e);
		} catch (NodeStoreException e) {
			logger.error("Data Store Exception", e);
		}
	}

	private void updateNodeConfiguration(HashMap<String, String> configuration)
			throws Exception {
		channelManager.setNodeConf(node, configuration);
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("configure");
	}
}