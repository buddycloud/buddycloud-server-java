package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class SubscriptionsResult extends PubSubElementProcessorAbstract {

	public SubscriptionsResult(ChannelManager channelManager) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean accept(Element elm) {
		// TODO Auto-generated method stub
		return false;
	}

}
