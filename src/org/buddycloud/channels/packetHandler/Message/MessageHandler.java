package org.buddycloud.channels.packetHandler.Message;

import java.util.Map;
import java.util.Set;

import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.packetHandler.APacketHandler;
import org.buddycloud.channels.packetHandler.IPacketHandler;
import org.buddycloud.channels.pubsub.Subscription;
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
			return;
		}
		
		Element items = event.element("items");
		String node   = items.attributeValue("node");
		
		Element item  = items.element("item");
		String id     = item.attributeValue("id");
		
		Element entry  = item.element("entry");
		
		// TODO create "verify entry" or something similart to verify that the entry is ok.
		
		if(!jedis.exists("node:" + node + ":subscribers")) {
			return;
		}
		
		jedis.sadd("node:" + node + ":itemset", id);
		jedis.lpush("node:" + node + ":itemlist", id);
		jedis.set("node:" + node + ":item:" + id, entry.asXML());
		
		Set<String> subscriberJIDs = jedis.smembers("node:" + node + ":subscribers");
		if(subscriberJIDs.isEmpty()) {
			System.out.println("Weird, there is no subscribers....");
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
