package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items;

import java.io.StringReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.queue.FederatedQueueManager;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class SpecialItemsGet extends PubSubElementProcessorAbstract {

	private static final Logger logger = Logger.getLogger(SpecialItemsGet.class);

	private static final String FIREHOSE = "/firehose";

	public SpecialItemsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		setChannelManager(channelManager);
		setOutQueue(outQueue);
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		request = reqIQ;
		PubSubElementProcessor processor = null;
        if (FIREHOSE == node) {
        	//processor = new FireHoseGet(outQueue, channelManager);
        } else {
        	featureNotImplementedError();
        	return;
        }
        processor.process(elm, actorJID, reqIQ, rsm);
	}

	private void featureNotImplementedError() throws InterruptedException {
		setErrorCondition(PacketError.Type.cancel, PacketError.Condition.feature_not_implemented);
	    outQueue.put(response);
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}
}