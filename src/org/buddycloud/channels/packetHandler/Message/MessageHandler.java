package org.buddycloud.channels.packetHandler.Message;

import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.buddycloud.channels.packetHandler.APacketHandler;
import org.buddycloud.channels.packetHandler.IPacketHandler;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.dom4j.Element;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

import redis.clients.jedis.Jedis;

public class MessageHandler extends APacketHandler implements IPacketHandler {
	
	public OutQueue outQueue;
	public ErrorQueue errorQueue;
	private Jedis jedis;
	
	private Logger LOGGER = Logger.getLogger(MessageHandler.class);
	
	public MessageHandler(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		this.outQueue   = outQueue;
		this.errorQueue = errorQueue;
		this.jedis      = jedis;
		
	}

	@Override
	public void ingestPacket(Packet p) {
		// TODO Auto-generated method stub
		Message msg = (Message)p;
		
		/**
		 * We'll be receiving something like this:
		 * http://buddycloud.org/wiki/XMPP_XEP#Notification_of_a_new.2Fupdated_channel_post
		 */
		
		Element event = msg.getChildElement("event", "http://jabber.org/protocol/pubsub#event");
		if(event == null) {
			LOGGER.debug("Did not find event element. Returning.");
			return;
		}
		
		Element items = event.element("items");
		if(items == null) {
			LOGGER.debug("Did not find items element. Returning.");
			return;
		}
		String node   = items.attributeValue("node");
		if(node == null) {
			LOGGER.debug("Did not find node element. Returning.");
			return;
		}
		
		String externalChannelServer = jedis.get("remove-node:" + node + ":jid");
		if(!msg.getFrom().toBareJID().equals(externalChannelServer)) {
			LOGGER.info("Received post to node from different jid than the one that is allowed (" + externalChannelServer + "). Returning.");
			return;
		}
		
		Element item  = items.element("item");
		if(item == null) {
			LOGGER.debug("Did not find item element. Returning.");
			return;
		}
		
		String id     = item.attributeValue("id");
		if(id == null) {
			LOGGER.debug("We did not have ID in the item element. Returning.");
			return;
		}
		
		Element entry  = item.element("entry");
		
		// TODO create "verify entry" or something similart to verify that the entry is ok.
		
		jedis.sadd("node:" + node + ":itemset", id);
		jedis.lpush("node:" + node + ":itemlist", id);
		jedis.set("node:" + node + ":item:" + id, entry.asXML());
		
		Set<String> subscriberJIDs = jedis.smembers("node:" + node + ":subscribers");
		if(subscriberJIDs.isEmpty()) {
			LOGGER.debug("Weird, there is no subscribers....");
			return;
		}
		
		msg.setFrom(msg.getTo());
		for (String subscriber : subscriberJIDs) {
			
			Map<String, String> subscription = jedis.hgetAll("node:" + node + ":subscriber:" + subscriber);
			
			/**
			 * TODO Check here if the user is blocked etc. 
			 */
			
			msg.setTo(subscriber);
			outQueue.put(msg.createCopy());
		}
		
	}
	
}
