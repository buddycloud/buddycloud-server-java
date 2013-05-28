package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class RecentItemsGet extends PubSubElementProcessorAbstract {

	private Element resultSetManagement;
	private String firstItem;
	private String lastItem;
	private int totalEntriesCount;
	
	private SimpleDateFormat sdf;
	
	private Date maxAge;
	private Integer maxItems;

	private static final Logger logger = Logger
			.getLogger(RecentItemsGet.class);

	public RecentItemsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		setChannelManager(channelManager);
		setOutQueue(outQueue);

		sdf = new SimpleDateFormat(Conf.DATE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		response = IQ.createResultIQ(reqIQ);
		request = reqIQ;
		actor = actorJID;
		node = elm.attributeValue("node");
		resultSetManagement = rsm;
		
		if (false == validStanza()) {
			outQueue.put(response);
			return;
		}
	}

	private boolean validStanza() {
		Element recentItems = request.getChildElement().element("recent-items");
		try {
			String max = recentItems.attributeValue("max");
			if (null == max) {
				createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, "max-required");
				return false;
			}
			maxItems = Integer.parseInt(max);
		    String since = recentItems.attributeValue("since");
		    if (null == since) {
				createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, "since-required");
				return false;
		    }
		    maxAge = sdf.parse(since);

		} catch (NumberFormatException e) {
			logger.error(e);
			createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, "invalid-max-value-provided");
			return false;	
		} catch (ParseException e) {
			createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, "invalid-since-value-provided");
            logger.error(e);
			return false;
		}
		return true;
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("recent-items");
	}
}