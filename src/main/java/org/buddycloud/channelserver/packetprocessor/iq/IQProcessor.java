package org.buddycloud.channelserver.packetprocessor.iq;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo.JabberDiscoInfo;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoitems.JabberDiscoItems;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.mam.MessageArchiveManagement;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.register.JabberRegister;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.buddycloud.channelserver.queue.UnknownFederatedPacketException;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class IQProcessor implements PacketProcessor<IQ> {

	private static final Logger logger = Logger.getLogger(IQProcessor.class);

	private Map<String, PacketProcessor<IQ>> processorsPerNamespace = new HashMap<String, PacketProcessor<IQ>>();
	private BlockingQueue<Packet> outQueue;
	private ChannelManager channelManager;
	private FederatedQueueManager federatedQueueManager;

	public IQProcessor(BlockingQueue<Packet> outQueue, Properties conf,
			ChannelManager channelManager,
			FederatedQueueManager federatedQueueManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
		this.federatedQueueManager = federatedQueueManager;

		JabberPubsub ps = new JabberPubsub(outQueue, conf, channelManager,
				federatedQueueManager);

		processorsPerNamespace.put(JabberDiscoItems.NAMESPACE_URI,
				new JabberDiscoItems(outQueue, conf, channelManager,
						federatedQueueManager));
		processorsPerNamespace.put(JabberDiscoInfo.NAMESPACE_URI,
				new JabberDiscoInfo(outQueue, conf, channelManager,
						federatedQueueManager));
		processorsPerNamespace.put(JabberRegister.NAMESPACE_URI,
				new JabberRegister(outQueue, conf, channelManager));
		processorsPerNamespace.put(JabberPubsub.NAMESPACE_URI, ps);
		processorsPerNamespace.put(JabberPubsub.NS_PUBSUB_OWNER, ps);
		processorsPerNamespace.put(MessageArchiveManagement.NAMESPACE_MAM,
				new MessageArchiveManagement(outQueue, channelManager));
	}

	@Override
	public void process(IQ packet) throws Exception {

		try {
			processPacket(packet);
		} catch (Exception e) {
			if (true == packet.getType().equals("result")) return;
			IQ reply = IQ.createResultIQ(packet);
			reply.setChildElement(packet.getChildElement().createCopy());
			reply.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.internal_server_error,
					org.xmpp.packet.PacketError.Type.wait);
			reply.setError(pe);

			this.outQueue.put(reply);
		}
		processPacket(packet);
	}

	private void processPacket(IQ packet) throws Exception,
			InterruptedException {
		if (null != packet.getChildElement()) {
			logger.debug("Finding IQ processor for namespace "
					+ packet.getChildElement().getNamespaceURI());

			PacketProcessor<IQ> namespaceProcessor = processorsPerNamespace
					.get(packet.getChildElement().getNamespaceURI());

			if (packet.getChildElement().getNamespaceURI() != null
					&& namespaceProcessor != null) {
				logger.trace("Using namespace processor: "
						+ namespaceProcessor.getClass().getName());
				namespaceProcessor.process(packet);
				return;

			}
		}
		// See if this was an externally sent packet
		try {
			federatedQueueManager.passResponseToRequester(packet);
			return;
		} catch (UnknownFederatedPacketException e) {
			logger.error(e);
		}
		logger.debug("Couldn't find processor for packet");

		if (packet.getType() == IQ.Type.set || packet.getType() == IQ.Type.get) {

			IQ reply = IQ.createResultIQ(packet);
			reply.setChildElement(packet.getChildElement().createCopy());
			reply.setType(Type.error);
			PacketError pe = new PacketError(
					org.xmpp.packet.PacketError.Condition.service_unavailable,
					org.xmpp.packet.PacketError.Type.cancel);
			reply.setError(pe);

			this.outQueue.put(reply);
			return;

		}
		logger.error("Could not handle packet " + packet.toXML());
	}
}