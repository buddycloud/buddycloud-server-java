package org.buddycloud.channelserver.packetprocessor.message.event;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.dom4j.Element;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class ConfigurationProcessor extends AbstractMessageProcessor  {
 
	private Helper helper;

	private static final Logger logger = Logger
			.getLogger(ConfigurationProcessor.class);

	public ConfigurationProcessor(BlockingQueue<Packet> outQueue,
			Properties configuration, ChannelManager channelManager) {
		super(channelManager, configuration, outQueue);
		this.helper = new Helper();
	}
	
	public void setConfigurationHelper(Helper helper) {
		this.helper = helper;
	}

	@Override
	public void process(Message packet) throws Exception {
		message = packet;

		if (true == channelManager.isLocalNode(node)) {
			return;
		}
		sendLocalNotifications();
		handleDataForm();
	}

	private void handleDataForm() throws NodeStoreException {
		Element configurationElement = message.getElement().element("event")
				.element("configuration");
		if (null == configurationElement) {
			return;
		}
		node = configurationElement.attributeValue("node");

		if (true == channelManager.isLocalNode(node)) {
			return;
		}
        setNodeConfiguration();
	}

	private void setNodeConfiguration() throws NodeStoreException {
		addRemoteNode();
		helper.parseEventUpdate(message);
		channelManager.setNodeConf(node, helper.getValues());	
	}

	private void addRemoteNode() {
        try { 
        	if (false == channelManager.nodeExists(node))
                channelManager.addRemoteNode(node); 
        } catch (NodeStoreException e) { 
        	logger.error(e);
        }
	}
}