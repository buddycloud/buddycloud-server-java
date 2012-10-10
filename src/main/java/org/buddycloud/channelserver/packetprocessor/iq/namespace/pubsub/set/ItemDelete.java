package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class ItemDelete extends PubSubElementProcessorAbstract {

	private static final Logger LOGGER = Logger.getLogger(ItemDelete.class);

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;

	public ItemDelete(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws InterruptedException, NodeStoreException {
		
		element = elm;
		request = reqIQ;
		response = IQ.createResultIQ(request);
		
		try {
	        if ((false == validNodeProvided())
	            || (false == nodeExists())) {
	        	outQueue.add(response);
	        }
		} catch (NodeStoreException e) {
			setErrorCondition(PacketError.Type.wait, PacketError.Condition.internal_server_error);
			outQueue.add(response);
		}
		
	}

	private boolean nodeExists() throws NodeStoreException {
		if ((false == channelManager.isLocalNode(node)) 
				|| (false == channelManager.nodeExists(node))) {
			setErrorCondition(PacketError.Type.cancel, PacketError.Condition.item_not_found);
			return false;
		}
		return true;
	}

	private boolean validNodeProvided() {
		node = element.attributeValue("node");
		if ((null != node) && (false == node.equals(""))) {
			return true;
		}
		response.setType(IQ.Type.error);
		Element nodeIdRequired = new DOMElement("nodeid-required",
				new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
		Element badRequest = new DOMElement(
				PacketError.Condition.bad_request.toXMPP(), new Namespace("",
						JabberPubsub.NS_XMPP_STANZAS));
		Element error = new DOMElement("error");
		error.addAttribute("type", PacketError.Type.modify.toString());
		error.add(badRequest);
		error.add(nodeIdRequired);
		response.setChildElement(error);
		return false;
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("retract");
	}

}
