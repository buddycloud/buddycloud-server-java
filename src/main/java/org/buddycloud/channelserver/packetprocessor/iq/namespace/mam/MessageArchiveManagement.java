package org.buddycloud.channelserver.packetprocessor.iq.namespace.mam;

import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo.DiscoInfoGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.IQ.Type;

public class MessageArchiveManagement implements PacketProcessor<IQ> {

	public static final String NAMESPACE = "urn:xmpp:archive#management";

	public static final String ELEMENT_NAME = "query";
	private static final Logger logger = Logger
			.getLogger(MessageArchiveManagement.class);
	private final BlockingQueue<Packet> outQueue;
	private ChannelManager channelManager;

	private IQ requestIq;

	public MessageArchiveManagement(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(IQ reqIQ) throws Exception {
		this.requestIq = reqIQ;

		if (false == channelManager.isLocalJID(requestIq.getFrom())) {
			this._sendNotHandledStanza();
			return;
		}

		if (false == isValidRequest())
			return;

		sendSubscriptionUpdates();
		sendAffiliationUpdated();
		sendItemUpdates();
		outQueue.put(IQ.createResultIQ(this.requestIq));
	}

	private boolean isValidRequest() {
		// TODO Auto-generated method stub
		return false;
	}

	private void sendItemUpdates() {
		// TODO Auto-generated method stub

	}

	private void sendAffiliationUpdated() {
		// TODO Auto-generated method stub

	}

	private void sendSubscriptionUpdates() {
		// TODO Auto-generated method stub

	}

	private void _sendNotHandledStanza() throws InterruptedException {
		IQ reply = IQ.createResultIQ(requestIq);
		reply.setChildElement(requestIq.getChildElement().createCopy());
		reply.setType(Type.error);
		PacketError pe = new PacketError(
				PacketError.Condition.service_unavailable,
				PacketError.Type.cancel);
		reply.setError(pe);

		outQueue.put(reply);
	}
}