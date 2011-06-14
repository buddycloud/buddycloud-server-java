package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.UUID;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.pubsub.Subscription;
import org.buddycloud.channels.pubsub.subscription.Type;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;

import redis.clients.jedis.Jedis;

public class JabberPubsub extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/pubsub";
	
	public JabberPubsub(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		super(outQueue, errorQueue, jedis);
		setProcessors.put(SetPubSub.ELEMENT_NAME, new SetPubSub());
		
	}
	
	private class SetPubSub implements IAction {

		public static final String ELEMENT_NAME = "pubsub";

		@Override
		public void process() {
			
			Element pubsub = reqIQ.getChildElement();
			@SuppressWarnings("unchecked")
			List<Element> elements = pubsub.elements();

			boolean handled = false;
			String feature = "";
			for (Element x : elements) {
				feature = x.getName(); 
				if (feature.equals("publish")) {
					this.publish(x);
					handled = true;
				} else if(feature.equals("subscribe")) {
					this.subscribe(x);
					handled = true;
				}
				break;
			}
			
			if (handled == false) {
				
				// <iq type='error'
				//	   from='pubsub.shakespeare.lit'
				//	   to='hamlet@denmark.lit/elsinore'
				//	   id='create1'>
				//	   <error type='cancel'>
				//	     <feature-not-implemented xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
				//       <unsupported xmlns='http://jabber.org/protocol/pubsub#errors' feature='create-nodes'/>
				//	   </error>
				// </iq>

				ErrorPacket ep = ErrorPacketBuilder.featureNotImplemented(reqIQ);
				Element unsupported = new DOMElement("unsupported",
						 							 new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
				unsupported.addAttribute("feature", feature);
				ep.addCondition(unsupported);
				
				errorQueue.put(ep);
				
			}
		}
		
		private void publish(Element elm) {
			
			String node = elm.attributeValue("node");
			if(node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Node attribute was missing when trying to publish an item.");
				errorQueue.put(ep);
				return;
			}
			
			
			// 7.1.3.3 Node Does Not Exist
			if(!jedis.sismember(JedisKeys.LOCAL_NODES, node)) {
				
				if(!jedis.sismember(JedisKeys.REMOTE_NODES, node)) {
					// 7.1.3.3 Node Does Not Exist
					// TODO, this is not a good place here. It is possible that we want to do 
					// discovery here and send the packet. All open nodes for everyone can exists.
					errorQueue.put(ErrorPacketBuilder.itemNotFound(reqIQ));
					return;
				}
				
				// TODO We do a post behalf of the sender here.
				
			} 
			
			
			Element item = elm.element("item");
			if(item == null) {
				ErrorPacket ep = ErrorPacketBuilder.badRequest(reqIQ);
				Element itemRequired = new DOMElement("item-required",
						                              new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
				ep.addCondition(itemRequired);
				return;
			}
			
			// TODO, add this:
			// 7.1.3.5 Bad Payload
			Element entry = item.element("entry");
			
			//String id = item.attributeValue("id");
			//if(id == null || id.equals("")) {
			String id = UUID.randomUUID().toString();
			//	item.addAttribute("id", id);
			//}
			
			Element idElm = entry.element("id");
			if(idElm == null) {
				idElm = entry.addElement("id");
			}
			idElm.setText(node + ":" + id);
			
			String DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
			SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
			sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
			
			Element publishedElm = entry.element("published");
			if(publishedElm == null) {
				publishedElm = entry.addElement("published");
			}
			publishedElm.setText(sdf.format(new Date()));
			
			Element updatedElm = entry.element("updated");
			if(updatedElm == null) {
				updatedElm = entry.addElement("updated");
			}
			updatedElm.setText(sdf.format(new Date()));
			
			/*
			 * All good, let's store the item.
			 */
			
			if(jedis.sadd("node:" + node + ":itemset", id) == 0) {
				ErrorPacket ep = ErrorPacketBuilder.conflict(reqIQ);
				ep.setMsg("ItemID '" + id + "' already exists. Item cannot be added.");
				errorQueue.put(ep);
				return;
			}
			
			jedis.lpush("node:" + node + ":itemlist", id);
			jedis.set("node:" + node + ":item:" + id, entry.asXML());
			
			/* 
			 * Success, let's reply as defined in 
			 * http://xmpp.org/extensions/xep-0060.html#publisher-publish - 7.1.2 Success Case
			 */
			
			Element pubsub = new DOMElement(ELEMENT_NAME, new org.dom4j.Namespace("", NAMESPACE_URI));
			
			Element publish = pubsub.addElement("publish");
			publish.addAttribute("node", node);
			
			Element newItem = publish.addElement("item");
			newItem.addAttribute("id", id);
			
			IQ result = IQ.createResultIQ(reqIQ);
			result.setChildElement(pubsub);
			
			outQueue.put(result);
			
			/*
			 * Let's send notifications.
			 */
			
			jedis.smembers("node:" + node + ":subscribers");
			
			Message msg = new Message();
			msg.setType(Message.Type.headline);
			msg.setID(id);
			Element event = msg.addChildElement("evet", JabberPubsubEvent.NAMESPACE_URI);
			Element items = event.addElement("items");
			items.addAttribute("node", node);
			Element i = items.addElement("item");
			i.add(entry.createCopy());
			
			System.out.println(msg.toXML());
			
			/*
			 * let's clean if we have too many items on the node.
			 */
		}
		
		private void subscribe(Element elm) {
			
			String node = elm.attributeValue("node");
			
			if(node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Node attribute was missing when trying to subscribe.");
				errorQueue.put(ep);
				return;
			}
			
			String jid = elm.attributeValue("jid");
			JID subsJID = new JID(jid);
			if(!subsJID.toBareJID().equals(reqIQ.getFrom().toBareJID())) {
				ErrorPacket ep = ErrorPacketBuilder.badRequest(reqIQ);
				Element invalidJid = new DOMElement("invalid-jid",
													new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
				ep.addCondition(invalidJid);
				ep.setMsg("JID was not passed as it should. E.g was not same as from in Bare form.");
				errorQueue.put(ep);
				return;
			}
			
			if(subsJID.getNode() != null && !jedis.sismember(JedisKeys.LOCAL_USERS, subsJID.toBareJID())) {
				errorQueue.put(ErrorPacketBuilder.registrationRequired(reqIQ));
				return;
			}
			
			String remoteChannelServer = null;
			String buddycloudPrefix = "";
			Namespace namespace = elm.getNamespaceForURI("http://buddycloud.org/v1");
			if(namespace != null) {
				buddycloudPrefix = namespace.getPrefix() + ":";
			}
			String actor = elm.attributeValue(buddycloudPrefix + "actor");
			if(actor == null && subsJID.getNode() == null) {
				
				/** 
				 * Case where we have subscription from other domain but no actor.
				 */
				
				ErrorPacket ep = ErrorPacketBuilder.badRequest(reqIQ);
				Element invalidJid = new DOMElement("invalid-jid",
													new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
				ep.addCondition(invalidJid);
				ep.setMsg("JID or actor was not passed as they should.");
				errorQueue.put(ep);
				return;
			} else if(actor == null) {
				actor = subsJID.toBareJID();
			} else {
				/**
				 * Must be coming from remote channel server.
				 */
				remoteChannelServer = reqIQ.getFrom().toBareJID();
			}
			
			JID actorJID = new JID(actor);
			if(actorJID.getDomain() == null || actorJID.getNode() == null ) {
				ErrorPacket ep = ErrorPacketBuilder.badRequest(reqIQ);
				Element invalidJid = new DOMElement("invalid-jid",
													new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
				ep.addCondition(invalidJid);
				ep.setMsg("Actor was not passed as it should.");
				errorQueue.put(ep);
				return;
			}
			
			if(!reqIQ.getFrom().toBareJID().equals(subsJID)) {
				// TODO check that fromJID is allowed to act behalf of subsJID.
				//
				// Because we are nice guys, we let everyone to subscribe now :)
			}
			
			// 7.1.3.3 Node Does Not Exist
			if(!jedis.sismember(JedisKeys.LOCAL_NODES, node)) {
				
				if(!jedis.sismember(JedisKeys.REMOTE_NODES, node)) {
					// We need to discover if the channel exists.  
					
					String[] splittedNode = node.split("/");
					
					// 0 1    2     3
					// /user/JID/something
					
					JID user = new JID(splittedNode[2]);
					
					IQ discoItemsGet = new IQ();
					String id = UUID.randomUUID().toString();
					discoItemsGet.setType(IQ.Type.get);
					discoItemsGet.setID(id);
					discoItemsGet.setFrom("channels.koski.com");
					discoItemsGet.setTo(user.getDomain());
					
					discoItemsGet.setChildElement("query", "http://jabber.org/protocol/disco#items");
					outQueue.put(discoItemsGet);
					
					Map <String, String> store = new HashMap<String, String>();
					store.put("type", "subscribe-items");
					store.put("id", reqIQ.getID());
					store.put("jid", reqIQ.getFrom().toString());
					store.put("node", node);
					
					jedis.hmset("store:" + id, store);
					
					return;
				}
				
				return;
			}
			
			// Here we do a local subscription.
			
			if(jedis.sadd("node:" + node + ":subscribers", actorJID.toBareJID()) == 0) {
				errorQueue.put(ErrorPacketBuilder.tooManySubscriptions(reqIQ));
				return;
			}
			
			Subscription sub = new Subscription(Type.unconfigured,
												org.buddycloud.channels.pubsub.affiliation.Type.publisher,
												remoteChannelServer);
			
			jedis.hmset("node:" + node + ":subscriber:" + actorJID.toBareJID(), sub.getAsMap());
			
			Element pubsub = new DOMElement(ELEMENT_NAME, new org.dom4j.Namespace("", NAMESPACE_URI));			
			Element subscription = pubsub.addElement("subscription");
			subscription.addAttribute("node", node);
			subscription.addAttribute("jid", reqIQ.getFrom().toBareJID());
			subscription.addAttribute("subscription", sub.getSubscription());
			
			if(actor != null) {
				pubsub.add(new org.dom4j.Namespace("bc", "http://buddycloud.org/v1"));
				subscription.addAttribute("bc:actor", actorJID.toBareJID());
			}
			
			IQ result = IQ.createResultIQ(reqIQ);
			result.setChildElement(pubsub);
			
			outQueue.put(result);
		}
	}
}
