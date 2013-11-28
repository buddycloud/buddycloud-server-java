package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.util.List;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class Configuration extends PubSubElementProcessorAbstract {

	private IQ request;
	private boolean ownerRequest;
	private String lastNode = "";

	private static final Logger logger = Logger
			.getLogger(Configuration.class);

	public Configuration(ChannelManager channelManager) {
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		this.request = reqIQ;

		if (-1 != request.getFrom().toString().indexOf("@")) {
			logger.debug("Ignoring result packet, only interested in stanzas "
					+ "from other buddycloud servers");
			return;
		}

	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("configure");
	}
}