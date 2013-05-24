package org.buddycloud.channelserver.packetprocessor.iq.namespace.mam;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.BlockingQueue;
import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.resultsetmanagement.ResultSet;

public class MessageArchiveManagement implements PacketProcessor<IQ> {

	public static final String NAMESPACE_MAM = "urn:xmpp:mam:tmp";
	public static final String NAMESPACE_FORWARDED = "urn:xmpp:forward:0";
	public static final String NAMESPACE_DELAY = "urn:xmpp:delay";

	public static final String ELEMENT_NAME = "query";
	private static final Logger logger = Logger
			.getLogger(MessageArchiveManagement.class);
	private final BlockingQueue<Packet> outQueue;
	private ChannelManager channelManager;
	
	private Message wrapper;

	public static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";

	SimpleDateFormat sdf;
    
	private Date startTimestamp;
	private Date endTimestamp = new Date();

	private IQ requestIq;
	private IQ reply;

	public MessageArchiveManagement(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
		
		sdf = new SimpleDateFormat(DATE_FORMAT);
	    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
	    sdf.setLenient(true);
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

		generateMessageWrapper();
		sendSubscriptionUpdates();
		sendAffiliationUpdated();
		sendItemUpdates();
		outQueue.put(IQ.createResultIQ(this.requestIq));
	}

	private void generateMessageWrapper() {
		wrapper = new Message();
		wrapper.setFrom(requestIq.getTo());
		wrapper.setTo(requestIq.getFrom());
		Element result = wrapper.addChildElement("result",
				NAMESPACE_MAM);
		result.addAttribute("id", requestIq.getID());
		Element forwarded = wrapper.addChildElement("forwarded", NAMESPACE_FORWARDED);
		Element delay = forwarded.addElement("delay", NAMESPACE_DELAY);
		delay.addAttribute("stamp", sdf.format(new Date()));
		Element message = forwarded.addElement("msg");
		message.addAttribute("type", Message.Type.headline.toString());
		message.addAttribute("to", requestIq.getFrom().toFullJID());
		message.addAttribute("from", requestIq.getTo().toString());
	}

	private boolean isValidRequest() throws InterruptedException {
		try {
			Element query = requestIq.getChildElement();
			startTimestamp = sdf.parse("1970-01-01T00:00:00Z");
			if (query.element("start") != null)
				startTimestamp = sdf.parse(query.elementText("start"));
			if (query.element("end") != null)
				endTimestamp = sdf.parse(query.elementText("end"));
			return true;
		} catch (ParseException e) {
			logger.error(e);
			sendErrorPacket(PacketError.Type.modify, PacketError.Condition.bad_request);
			return false;
		}
	}

	private void sendItemUpdates() {

	}

	private void sendAffiliationUpdated() {
		try {
			ResultSet<NodeAffiliation> changes = channelManager.getAffiliationChanges(requestIq.getFrom(), endTimestamp, endTimestamp);
			if (0 == changes.size()) return;
			Message notification = wrapper.createCopy();
			Element forwarded = notification.getElement().element("forwarded");
			
			Element event = forwarded.addElement("event");
			Element affiliations = event.addElement("affiliations");
			Element affiliation  = affiliations.addElement("affiliation");
			event.addNamespace("", JabberPubsub.NS_PUBSUB_EVENT);
			
			for (NodeAffiliation change : changes) {
				affiliations.addAttribute("node", change.getNodeId());
				affiliation.addAttribute("jid", change.getUser().toBareJID());
				affiliation.addAttribute("affiliation", change.getAffiliation().toString());
				forwarded.element("delay").addAttribute("stamp", sdf.format(change.getLastUpdated()));
				outQueue.put(notification.createCopy());
			}
		} catch (NodeStoreException e) {
			logger.error(e);
		} catch (InterruptedException e) {
			logger.error(e);
		}
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