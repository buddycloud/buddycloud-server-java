package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoitems;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo.DiscoInfoGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class DiscoItemsGet implements PacketProcessor<IQ> {

	public static final String ELEMENT_NAME = "query";
	private static final Logger logger = Logger.getLogger(DiscoInfoGet.class);
	private final BlockingQueue<Packet> outQueue;
	private String node;

	private IQ requestIq;
	private IQ response;

	private FederatedQueueManager federatedQueueManager;
	private ChannelManager channelManager;

	public DiscoItemsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager,
			FederatedQueueManager federatedQueueManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
		this.federatedQueueManager = federatedQueueManager;
	}

	@Override
	public void process(IQ reqIQ) throws Exception {
		this.requestIq = reqIQ;

		this.response = IQ.createResultIQ(this.requestIq);

		try {
			if (null == requestIq.getElement().element("query")
					.attributeValue("node")) {
				addItems();
			} else {
				setErrorCondition(PacketError.Type.cancel,
						PacketError.Condition.feature_not_implemented);
			}
		} catch (NodeStoreException e) {
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
		}
		outQueue.add(response);
	}

	private void addItems() throws NodeStoreException {
		ArrayList<String> nodes = channelManager.getNodeList();

		Element query = response.getElement().addElement("query");
		query.addNamespace("", JabberDiscoItems.NAMESPACE_URI);
		for (String node : nodes) {
			if (false == channelManager.isLocalNode(node))
				continue;
			Element item = query.addElement("item");
			item.addAttribute("node", node);
			item.addAttribute("jid", Configuration.CONFIGURATION_SERVER_DOMAIN);
		}
	}

	private void setErrorCondition(Type type, Condition condition) {
		response.setType(IQ.Type.error);
		PacketError error = new PacketError(condition, type);
		response.setError(error);
	}
}