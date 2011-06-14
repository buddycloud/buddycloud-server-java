package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.ArrayList;
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

public class JabberDiscoInfo extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/disco#info";
	
	public JabberDiscoInfo(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		super(outQueue, errorQueue, jedis);
		
		getProcessors.put(GetQuery.ELEMENT_NAME, new GetQuery());

		resultProcessors.put(ResultQuery.ELEMENT_NAME, new ResultQuery());
		
	}

	
	private class GetQuery implements IAction {

		public static final String ELEMENT_NAME = "query";
		
		@Override
		public void process() {
			
			IQ result = IQ.createResultIQ(reqIQ);
			
			Element query = result.setChildElement(ELEMENT_NAME, JabberDiscoInfo.NAMESPACE_URI);
			query.addElement("identity")
				 .addAttribute("category", "pubsub")
				 .addAttribute("type", "channels")
				 .addAttribute("name", "Koski's buddycloud channel server!");
			
			query.addElement("feature")
				 .addAttribute("var", "http://jabber.org/protocol/disco#info");
			
			outQueue.put(result);
			
		}
		
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
			
			@SuppressWarnings("unchecked")
			List<Element> identities = reqIQ.getChildElement().elements("identity");
			
			boolean isBcComponent = false;
			for(Element identity : identities) {
				if("pubsub".equals(identity.attributeValue("category")) && 
				   "channels".equals(identity.attributeValue("type"))) {
					isBcComponent = true;
				}
			}
			
			if(isBcComponent) {
				
				// We found bc component! Let's subscribe :-)
				
				String id = UUID.randomUUID().toString();
				
				IQ subscribe = new IQ();
				subscribe.setType(IQ.Type.set);
				subscribe.setID(id);
				subscribe.setTo(reqIQ.getFrom().toString());
				
				Element pubsub = subscribe.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
				pubsub.add(new org.dom4j.Namespace("bc", "http://buddycloud.org/v1"));
				pubsub.addElement("subscribe")
				      .addAttribute("node", store.get("node"))
				      .addAttribute("jid", reqIQ.getTo().toString())
				      .addAttribute("bc:actor", new JID(store.get("jid")).toBareJID());
				
				store.put("type", "subscribe");
				
				jedis.hmset("store:" + id, store);
				jedis.del("store:" + reqIQ.getID());
				
				outQueue.put(subscribe);
				
				return;
			}
			
			/*
			 * Component was not a buddycloud channel component. Let's look for another one.
			 */
			ArrayList<String> itemsToBeDiscovered = (ArrayList<String>) Helpers.stringToCollection(store.get("components"));
			
			if(itemsToBeDiscovered.isEmpty()) {
				
				/* 
				 * TODO, fallback to buddycloud.com here. Maybe. :-)
				 */
				
				IQ originalReq = new IQ();
				originalReq.setType(IQ.Type.set);
				originalReq.setID(store.get("id"));
				originalReq.setFrom(store.get("jid"));
				originalReq.setTo("channels.koski.com");
				
				if("subscribe-info".equals(store.get("type"))) {
					ErrorPacket ep = ErrorPacketBuilder.itemNotFound(originalReq);
					ep.setMsg("No bc components found from remove server (" + reqIQ.getFrom().toBareJID() + ") when doing disco#infos to them.");
					errorQueue.put(ErrorPacketBuilder.itemNotFound(originalReq));
				} else {
					ErrorPacket ep = ErrorPacketBuilder.internalServerError(originalReq);
					ep.setMsg("We had a type of store item in disco#info that was not yet handled: '" + store.get("type") + "'.");
					errorQueue.put(ep);
				}
				
				return;
			}
			
			String firstComponentJid = itemsToBeDiscovered.remove(0);
			
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
