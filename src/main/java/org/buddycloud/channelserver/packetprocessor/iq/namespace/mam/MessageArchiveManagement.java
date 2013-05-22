package org.buddycloud.channelserver.packetprocessor.iq.namespace.mam;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.BlockingQueue;
import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;

public class MessageArchiveManagement implements PacketProcessor<IQ> {

	public static final String NAMESPACE = "urn:xmpp:archive#management";

	public static final String ELEMENT_NAME = "query";
	private static final Logger logger = Logger
			.getLogger(MessageArchiveManagement.class);
	private final BlockingQueue<Packet> outQueue;
	private ChannelManager channelManager;

	public static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.S'Z'";
	SimpleDateFormat sdf;
    
	private Date startTimestamp;
	private Date endTimestamp;

	private IQ requestIq;
	private IQ reply;

	public MessageArchiveManagement(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
		
		sdf = new SimpleDateFormat(DATE_FORMAT);
	    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
	}

	@Override
	public void process(IQ reqIQ) throws Exception {
		requestIq = reqIQ;
		reply = IQ.createResultIQ(requestIq);

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

	private boolean isValidRequest() throws InterruptedException {
		try {
			Element query = requestIq.getChildElement();
			
			startTimestamp = sdf.parse("1970-01-01T00:00:00");
			if (query.attribute("start") != null) {
				startTimestamp = sdf.parse(query.attributeValue("start"));
			}
			endTimestamp = new Date();
			if (query.attribute("end") != null) {
				endTimestamp = sdf.parse(query.attributeValue("end"));
			}
			return true;
		} catch (ParseException e) {
			logger.error(e);
			sendErrorPacket(PacketError.Type.modify, PacketError.Condition.bad_request);
			return false;
		}
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
		sendErrorPacket(PacketError.Type.cancel, PacketError.Condition.service_unavailable);
	}


	private void sendErrorPacket(PacketError.Type type,
			Condition condition) throws InterruptedException {
		reply.setChildElement(requestIq.getChildElement().createCopy());
		reply.setType(Type.error);
		PacketError pe = new PacketError(condition, type);
		reply.setError(pe);
		outQueue.put(reply);
	}}