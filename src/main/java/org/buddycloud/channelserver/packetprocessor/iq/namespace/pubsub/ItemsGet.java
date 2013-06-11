package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.SpecialItemsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.UserItemsGet;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class ItemsGet extends PubSubElementProcessorAbstract {

	private UserItemsGet userItems;
	private SpecialItemsGet specialItems;

	public ItemsGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		setOutQueue(outQueue);
		setChannelManager(channelManager);
		userItems = new UserItemsGet(outQueue, channelManager);
		specialItems = new SpecialItemsGet(outQueue, channelManager);
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		request = reqIQ;

		String node = elm.attributeValue("node");
		if (null == node) {
			createExtendedErrorReply(PacketError.Type.modify,
					PacketError.Condition.bad_request, "nodeid-required");
			outQueue.put(response);
			return;
		}
		
		if (-1 == node.indexOf("@")) {
			specialItems.process(elm, actorJID, reqIQ, rsm);
			return;
		}
		userItems.process(elm, actorJID, reqIQ, rsm);
	}

	@Override
	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}

}
