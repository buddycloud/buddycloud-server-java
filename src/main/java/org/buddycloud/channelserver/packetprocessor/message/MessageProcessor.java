package org.buddycloud.channelserver.packetprocessor.message;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.message.event.AbstractMessageProcessor;
import org.buddycloud.channelserver.packetprocessor.message.event.AffiliationProcessor;
import org.buddycloud.channelserver.packetprocessor.message.event.ConfigurationProcessor;
import org.buddycloud.channelserver.packetprocessor.message.event.ItemsProcessor;
import org.buddycloud.channelserver.packetprocessor.message.event.RetractItemProcessor;
import org.buddycloud.channelserver.packetprocessor.message.event.SubscriptionProcessor;
import org.dom4j.Element;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class MessageProcessor implements PacketProcessor<Message> {

	private static final Logger logger = Logger
			.getLogger(MessageProcessor.class);

	private final BlockingQueue<Packet> outQueue;
	private ChannelManager channelManager;
	private Message message;
	private Properties configuration;

	private Element event;

	public static final String ITEMS = "items";
	public static final String SUBSCRIPTION = "subscription";
	private static final String AFFILIATIONS = "affiliations";
	private static final String CONFIGURATION = "configuration";

	public MessageProcessor(BlockingQueue<Packet> outQueue, Properties conf,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
		this.configuration = conf;
	}

	@Override
	public void process(Message packet) throws Exception {

		message = packet;
		if (false == message.getType().equals(Message.Type.headline)) {
			return;
		}
		event = message.getElement().element("event");
		if (null != event) {
			processEventContent(((Element) event.elements().get(0)).getName());
			return;
		}
		logger.debug("Unsupported message type: " + packet.toXML());
		throw new UnsupportedOperationException("Unknown message type", null);
	}

	private void processEventContent(String name) throws Exception {

		logger.info("Processing event content type: '" + name + "'");
		PacketProcessor<Message> handler = null;
		if (name.equals(ITEMS)) {
			handler = processItems();
		} else if (true == name.equals(SUBSCRIPTION)) {
			handler = new SubscriptionProcessor(outQueue, configuration,
					channelManager);
		} else if (true == name.equals(AFFILIATIONS)) {
			handler = new AffiliationProcessor(outQueue, configuration,
					channelManager);
		} else if (true == name.equals(CONFIGURATION)) {
			handler = new ConfigurationProcessor(outQueue, configuration,
					channelManager);
		}
		if (null == handler) {
			throw new UnknownEventContentException("Unknown event content '"
					+ name + "'");
		}
		handler.process(message);
	}

	private AbstractMessageProcessor processItems() {
		Element item = event.element("items").element("item");
		if (null != item)
			return new ItemsProcessor(outQueue, configuration, channelManager);
		Element retract = event.element("items").element("retract");
		if (null != retract)
			return new RetractItemProcessor(outQueue, configuration,
					channelManager);
		return null;
	}
}