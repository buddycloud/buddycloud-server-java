package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.QName;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class Search implements PacketProcessor<IQ> {
	
	public static final String ELEMENT_NAME = "query";
	private static final Logger logger = Logger.getLogger(Search.class);
	
	public static final String NAMESPACE_URI = "jabber:iq:search";

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;
	private IQ request;

	private SearchGet searchGet;
	private SearchSet searchSet;

	public Search(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
		
		this.searchGet = new SearchGet(outQueue, channelManager);
		this.searchSet = new SearchSet(outQueue, channelManager);
	}

	@Override
	public void process(IQ reqIQ) throws Exception {
		request = reqIQ;
       
		if (request.getType().equals("get")) {
			
		} else if (request.getType().equals("set")) {
			
		}
		IQ response = IQ.createResultIQ(request);
		response.setType(IQ.Type.error);
		PacketError error = new PacketError(
				PacketError.Condition.bad_request,
				PacketError.Type.modify
		);
		response.setError(error);
		outQueue.put(response);
	}
}