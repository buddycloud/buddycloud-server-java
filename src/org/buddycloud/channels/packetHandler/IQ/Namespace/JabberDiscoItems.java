package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.utils.Helpers;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;

import redis.clients.jedis.Jedis;

public class JabberDiscoItems extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/disco#items";
	
	public JabberDiscoItems(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		super(outQueue, errorQueue, jedis);
		resultProcessors.put(ResultQuery.ELEMENT_NAME, new ResultQuery());
		
	}

	private class ResultQuery implements IAction {

		public static final String ELEMENT_NAME = "query";
		
		@Override
		public void process() {
			
			/**
			 * So we have a result query. 
			 * 
			 * First we need to check if we have anything in our store for this result.
			 */

			Map<String, String> store = jedis.hgetAll("store:" + reqIQ.getID());
			
			if(store == null || store.isEmpty()) {
				return;
			}
			
			/*
			 * Ok, this means we want to discover for some reason. So let's look for the 
			 * buddycloud server.
			 */
			
			// Let's see if we have any items.
			
			@SuppressWarnings("unchecked")
			List<Element> items = reqIQ.getChildElement().elements("item");
			
			LinkedList<String> itemsToBeDiscovered = new LinkedList<String>();
			for(Element item : items) {
				JID jid = new JID(item.attributeValue("jid"));
				itemsToBeDiscovered.add(jid.toBareJID());
			}
			
			if(itemsToBeDiscovered.isEmpty()) {
				
				// If we are here, it means that there is no items to be discovered
				// on the remove server we wanted to do an action with.
				//
				// At the moment only possibilities are subscribe so let's answer not found then.
				
				IQ originalReq = new IQ();
				originalReq.setType(IQ.Type.set);
				originalReq.setID(store.get("id"));
				originalReq.setFrom(store.get("jid"));
				originalReq.setTo("channels.koski.com");
				
				if("subscribe-items".equals(store.get("type"))) {
					ErrorPacket ep = ErrorPacketBuilder.itemNotFound(originalReq);
					ep.setMsg("No items found from remove server (" + reqIQ.getFrom().toBareJID() + ")when doing disco#items.");
					errorQueue.put(ErrorPacketBuilder.itemNotFound(originalReq));
				} else {
					ErrorPacket ep = ErrorPacketBuilder.internalServerError(originalReq);
					ep.setMsg("We had a type of store item that was not yet handled: '" + store.get("type") + "'.");
					errorQueue.put(ep);
				}
				
				return;
			}
			
			String firstComponentJid = itemsToBeDiscovered.poll();
			
			store.put("components", Helpers.collectionToString(itemsToBeDiscovered));
			store.put("type", "subscribe-info");
			
			String id = UUID.randomUUID().toString();
			jedis.hmset("store:" + id, store);
			jedis.del("store:" + reqIQ.getID());
			
			IQ discoInfoIQ = new IQ();
			discoInfoIQ.setTo(firstComponentJid);
			discoInfoIQ.setID(id);
			discoInfoIQ.setType(Type.get);
			
			discoInfoIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
			
			outQueue.put(discoInfoIQ);
			
		}
		
	}
	
}
