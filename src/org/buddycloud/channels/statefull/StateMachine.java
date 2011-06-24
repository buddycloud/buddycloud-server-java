package org.buddycloud.channels.statefull;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberDiscoInfo;
import org.buddycloud.channels.packetHandler.IQ.Namespace.JabberPubsub;
import org.buddycloud.channels.pubsub.Subscription;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.utils.Helpers;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;

import redis.clients.jedis.Jedis;

public class StateMachine {

	private Jedis jedis;
	private OutQueue outQueue;
	private ErrorQueue errorQueue;
	
	public StateMachine(Jedis jedis, OutQueue outQueue, ErrorQueue errorQueue) {
		this.jedis = jedis;
		this.outQueue = outQueue;
		this.errorQueue = errorQueue;
	}
	
	public void ingest(IQ iq) {
		
		Map<String, String> store = jedis.hgetAll("store:" + iq.getID());
		
		if(store == null || store.isEmpty()) {
			System.out.println("Could not recover state from store with key 'store:" + iq.getID() + "'.");
			return;
		}
		
		State state = new State(store);
		
		/**
		 * This is just a mockup!
		 * 
		 * I'll move them to proper classes when i have time.
		 * Now I have only 5 mins.
		 */
		if(state.getState().equals(State.STATE_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT)) {
			
			@SuppressWarnings("unchecked")
			List<Element> items = iq.getChildElement().elements("item");
			
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
				
				if("subscribe-items".equals(store.get(State.KEY_STATE))) {
					ErrorPacket ep = ErrorPacketBuilder.itemNotFound(originalReq);
					ep.setMsg("No items found from remove server (" + iq.getFrom().toBareJID() + ") when doing disco#items.");
					errorQueue.put(ErrorPacketBuilder.itemNotFound(originalReq));
				} else {
					ErrorPacket ep = ErrorPacketBuilder.internalServerError(originalReq);
					ep.setMsg("We had a type of store item that was not yet handled: '" + store.get(State.KEY_STATE) + "'.");
					errorQueue.put(ep);
				}
				
				return;
			}
			
			String firstComponentJid = itemsToBeDiscovered.poll();
			
			store.put("components", Helpers.collectionToString(itemsToBeDiscovered));
			store.put(State.KEY_STATE, State.STATE_DISCO_INFO_TO_COMPONENTS);
			
			String id = UUID.randomUUID().toString();
			jedis.hmset("store:" + id, store);
			jedis.del("store:" + iq.getID());
			
			IQ discoInfoIQ = new IQ();
			discoInfoIQ.setTo(firstComponentJid);
			discoInfoIQ.setID(id);
			discoInfoIQ.setType(Type.get);
			
			discoInfoIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
			
			outQueue.put(discoInfoIQ);
			
		} else if(state.getState().equals(State.STATE_DISCO_INFO_TO_COMPONENTS)) {
			
			@SuppressWarnings("unchecked")
			List<Element> identities = iq.getChildElement().elements("identity");
			
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
				subscribe.setTo(iq.getFrom().toString());
				
				Element pubsub = subscribe.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
				//pubsub.add(new org.dom4j.Namespace("bc", "http://buddycloud.org/v1"));
				pubsub.addElement("subscribe")
				      .addAttribute("node", store.get("node"))
				      .addAttribute("jid", iq.getTo().toString());
				      //.addAttribute("bc:actor", new JID(store.get("jid")).toBareJID());
				pubsub.addElement("actor", "http://buddycloud.org/v1")
				      .setText(new JID(store.get("jid")).toBareJID());
				
				store.put(State.KEY_STATE, State.STATE_SUBSCRIBE);
				
				jedis.hmset("store:" + id, store);
				jedis.del("store:" + iq.getID());
				
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
				
				if(State.STATE_DISCO_INFO_TO_COMPONENTS.equals(store.get(State.KEY_STATE))) {
					ErrorPacket ep = ErrorPacketBuilder.itemNotFound(originalReq);
					ep.setMsg("No bc components found from remove server (" + iq.getFrom().toBareJID() + ") when doing disco#infos to them.");
					errorQueue.put(ErrorPacketBuilder.itemNotFound(originalReq));
				} else {
					ErrorPacket ep = ErrorPacketBuilder.internalServerError(originalReq);
					ep.setMsg("We had a type of store item in disco#info that was not yet handled: '" + store.get(State.KEY_STATE) + "'.");
					errorQueue.put(ep);
				}
				
				return;
			}
			
			String firstComponentJid = itemsToBeDiscovered.remove(0);
			
			store.put("components", Helpers.collectionToString(itemsToBeDiscovered));
			
			String id = UUID.randomUUID().toString();
			jedis.hmset("store:" + id, store);
			jedis.del("store:" + iq.getID());
			
			IQ discoInfoIQ = new IQ();
			discoInfoIQ.setTo(firstComponentJid);
			discoInfoIQ.setID(id);
			discoInfoIQ.setType(Type.get);
			
			discoInfoIQ.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
			
			outQueue.put(discoInfoIQ);
		
		} else if(state.getState().equals(State.STATE_SUBSCRIBE)) {
			
			String node = state.getStore().get(State.KEY_NODE);
			String jid  = state.getStore().get(State.KEY_JID);
			String id   = state.getStore().get(State.KEY_ID);
			
			jedis.sadd(JedisKeys.REMOTE_NODES, node);
			jedis.set("remove-node:" + node + ":jid", iq.getFrom().toBareJID());
			
			IQ result = new IQ();
			result.setID(id);
			result.setType(Type.result);
			result.setTo(jid);
			
			jedis.sadd("node:" + node + ":subscribers", result.getTo().toBareJID());
			
			Subscription sub = new Subscription(org.buddycloud.channels.pubsub.subscription.Type.unconfigured,
												org.buddycloud.channels.pubsub.affiliation.Type.publisher,
												iq.getFrom().toBareJID());
			
			jedis.hmset("node:" + node + ":subscriber:" + result.getTo().toBareJID(), sub.getAsMap());
			
			Element pubsub = new DOMElement("pubsub", new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));			
			Element subscription = pubsub.addElement("subscription");
			subscription.addAttribute("node", node);
			subscription.addAttribute("jid", result.getTo().toBareJID());
			subscription.addAttribute("subscription", sub.getSubscription());
			
			result.setChildElement(pubsub);
			
			jedis.del("store:" + iq.getID());
			
			outQueue.put(result);
			
		} else if(state.getState().equals(State.STATE_PUBLISH)) {
			
			String jid  = state.getStore().get(State.KEY_JID);
			String id   = state.getStore().get(State.KEY_ID);
			
			IQ copy = iq.createCopy();
			copy.setID(id);
			copy.setFrom(copy.getTo());
			copy.setTo(jid);
			
			jedis.del("store:" + iq.getID());
			
			outQueue.put(copy);
			
		} else {
			System.out.println("Did not found handler for state '" + state.getState() + "'.");
		}
		
	}
	
}
