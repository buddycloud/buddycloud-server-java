package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.NodeItemsGet;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.items.SpecialItemsGet;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class ItemsGet extends PubSubElementProcessorAbstract {

    private NodeItemsGet userItems;
    private SpecialItemsGet specialItems;

    public ItemsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        setOutQueue(outQueue);
        setChannelManager(channelManager);
        userItems = new NodeItemsGet(outQueue, channelManager);
        specialItems = new SpecialItemsGet(outQueue, channelManager);

        acceptedElementName = XMLConstants.ITEMS_ELEM;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        request = reqIQ;

        String node = elm.attributeValue(XMLConstants.NODE_ATTR);
        if (null == node) {
            createExtendedErrorReply(PacketError.Type.modify, PacketError.Condition.bad_request, XMLConstants.NODE_ID_REQUIRED);
            outQueue.put(response);
            return;
        }

        if (-1 == node.indexOf("@")) {
            specialItems.process(elm, actorJID, reqIQ, rsm);
            return;
        }
        userItems.process(elm, actorJID, reqIQ, rsm);
    }
}
