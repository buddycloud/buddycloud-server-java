package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo;

import java.util.HashMap;
import java.util.List;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.buddycloud.channelserver.queue.UnknownFederatedPacketException;
import org.dom4j.Element;
import org.xmpp.component.ComponentException;
import org.xmpp.packet.IQ;

public class DiscoResult implements PacketProcessor<IQ> {

	public static final String ELEMENT_NAME = "query";
	private static final Logger logger = Logger.getLogger(DiscoResult.class);
	private String node;
	private IQ requestIq;
	
	private FederatedQueueManager federatedQueueManager;
	private ChannelManager channelManager;
	private Helper helper = new Helper(channelManager);

	public DiscoResult(ChannelManager channelManager,
			FederatedQueueManager federatedQueueManager) {
		this.channelManager = channelManager;
		this.federatedQueueManager = federatedQueueManager;
	}

	public void process(IQ reqIQ) throws Exception {
		requestIq = reqIQ;
		node = requestIq.getElement().element("query")
				.attributeValue("node");
		if ((null == node) && federatedQueueManager.isFederatedDiscoInfoRequest(
				reqIQ.getID())) {
			federatedRequest();
			return;
		}
		try {
			federatedQueueManager.passResponseToRequester(requestIq);
			processConfigurationSettings();
		} catch (UnknownFederatedPacketException e) {
			logger.error(e);
		}
	}

	@SuppressWarnings("unchecked")
	private void federatedRequest() throws ComponentException {
		List<Element> identities = requestIq.getChildElement().elements(
				"identity");
		federatedQueueManager.processDiscoInfoResponse(requestIq.getFrom(),
				requestIq.getID(), identities);
	}

	private void processConfigurationSettings() throws NodeStoreException {
		if (node == null) {
			return;
		}
		if (!channelManager.nodeExists(node)) {
			channelManager.addRemoteNode(node);
		}
		this.helper.parseDiscoInfo(requestIq);
		HashMap<String, String> configuration = this.helper.getValues();
		channelManager.setNodeConf(node, configuration);
	}
}