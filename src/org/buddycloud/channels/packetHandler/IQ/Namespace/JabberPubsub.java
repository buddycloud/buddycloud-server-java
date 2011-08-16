package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.io.StringReader;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.UUID;

import org.buddycloud.channels.entry.Validator;
import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.pubsub.Subscription;
import org.buddycloud.channels.pubsub.subscription.Type;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.State;
import org.buddycloud.channels.statefull.StateMachine;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;

import redis.clients.jedis.Jedis;

public class JabberPubsub extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/pubsub";
	
	public JabberPubsub(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		super(outQueue, errorQueue, jedis);
		setProcessors.put(SetPubSub.ELEMENT_NAME, new SetPubSub());
		getProcessors.put(GetPubSub.ELEMENT_NAME, new GetPubSub());
		resultProcessors.put(ResultPubSub.ELEMENT_NAME, new ResultPubSub());
		errorProcessors.put(ErrorPubSub.ELEMENT_NAME, new ErrorPubSub());
	}
	
	private class SetPubSub implements IAction {

		public static final String ELEMENT_NAME = "pubsub";

		@Override
		public void process() {
			
			Element pubsub = reqIQ.getChildElement();
			
			//Let's get the possible actor
			String actor = null;
			if(pubsub.element("actor") != null) {
				JID actorJID = new JID(pubsub.element("actor").getTextTrim());
				/**
				 * TODO validate here that the JID is somehow sane.
				 *      We could check that the domains are the same etc.
				 */
				actor = actorJID.toBareJID();
			}
			
			@SuppressWarnings("unchecked")
			List<Element> elements = pubsub.elements();

			boolean handled = false;
			String feature = "";
			for (Element x : elements) {
				feature = x.getName();
				if(feature.equals("create")) {
					this.create(x);
					handled = true;
				} else if (feature.equals("publish")) {
					this.publish(x, actor);
					handled = true;
				} else if(feature.equals("subscribe")) {
					this.subscribe(x, actor);
					handled = true;
				} else if(feature.equals("retract")) {
					this.retract(x);
					handled = true;
				}
				//break;
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
		
		private void create(Element elm) {
			
			if(!jedis.sismember(JedisKeys.LOCAL_USERS, reqIQ.getFrom().toBareJID())) {
				errorQueue.put(ErrorPacketBuilder.registrationRequired(reqIQ));
				return;
			}
			
			String node = elm.attributeValue("node");
			if (node == null || node.equals("")) {
				// TODO:
				// Launch error
				// <iq type='error'
				// from='pubsub.shakespeare.lit'
				// to='hamlet@denmark.lit/elsinore'
				// id='create2'>
				// <error type='modify'>
				// <not-acceptable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
				// <nodeid-required
				// xmlns='http://jabber.org/protocol/pubsub#errors'/>
				// </error>
				// </iq>

				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Tried to create a node without passing node ID.");
				errorQueue.put(ep);
				return;
			}
			
			if(!node.startsWith("/user/")) {
				ErrorPacket ep = ErrorPacketBuilder.badRequest(reqIQ);
				
				Element invalidJid = new DOMElement("invalid-nodename",
						new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
				ep.addCondition(invalidJid);
				
				ep.setMsg("Nodename must start with /user/.");
				errorQueue.put(ep);
				return;
			}
			
			if (jedis.sadd(JedisKeys.LOCAL_NODES, node) == 0) {
				
				// We could not add the node, it means it already exists.
				// <iq type='error'
				// from='pubsub.shakespeare.lit'
				// to='hamlet@denmark.lit/elsinore'
				// id='create1'>
				// <error type='cancel'>
				// <conflict xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
				// </error>
				// </iq>

				ErrorPacket ep = ErrorPacketBuilder.conflict(reqIQ);
				ep.setMsg("User tried to create a node that already existed.");
				errorQueue.put(ep);
				return;
			}
			
			Subscription sub = new Subscription(Type.subscribed,
					org.buddycloud.channels.pubsub.affiliation.Type.owner,
					null);

			jedis.hmset("node:" + node + ":subscriber:" + reqIQ.getFrom().toBareJID(), sub.getAsMap());
			jedis.set("node:" + node + ":owner", reqIQ.getFrom().toBareJID());
			jedis.sadd(reqIQ.getFrom().toBareJID() + ":subs", node);
			
			String DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
			SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
			sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
			
			Map <String, String> conf = new HashMap<String, String>();
			conf.put("pubsub#type", "http://www.w3.org/2005/Atom");
			conf.put("pubsub#title", "");
			conf.put("pubsub#description", "");
			conf.put("pubsub#publish_model", "publishers");
			conf.put("pubsub#access_model", "open");
			conf.put("pubsub#creation_date", sdf.format(new Date()));
			conf.put("pubsub#owner", reqIQ.getFrom().toBareJID());
			conf.put("pubsub#default_affiliation", org.buddycloud.channels.pubsub.affiliation.Type.publisher.toString());
			conf.put("pubsub#num_subscribers", "1");
			conf.put("pubsub#notify_config", "1");
			
			jedis.hmset("node:" + node + ":conf", conf);
			
			jedis.sadd("node:" + node + ":subscribers", reqIQ.getFrom().toBareJID());
			
			// Success.
			// BUT since pubsub create can have configure in it as well...
			// http://xmpp.org/extensions/xep-0060.html#owner-create-and-configure
			// This is a big of a hack.
			//
			// THIS WHOLE THING IS JUST A MOCKUP NOW! i'M JUST GETTING THIS RUNNING
			// AND WILL AFTER DO A WHOLE REWRITE AFTER!
			//
			Set <String> configurableValues = new HashSet<String>();
			configurableValues.add("pubsub#title");
			configurableValues.add("pubsub#description");
			Element pubsub = reqIQ.getChildElement();
			List<Element> elements = pubsub.elements();
			for (Element configure : elements) {
				if (!configure.getName().equals("configure")) {
					continue;
				}
				
				Element x = configure.element("x");
				
				if(x == null) {
					continue;
				}
				
				List<Element> elmFields = x.elements();
				for (Element elmField : elmFields) {
					
					if(!"field".equals(elmField.getName())) {
						continue;
					}
					
					String var = elmField.attributeValue("var");
					
					Element value = elmField.element("value");
					if(value == null) {
						continue;
					}
					
					String val = value.getTextTrim(); 
					
					if(!configurableValues.contains(elmField.attributeValue("var"))) {
						continue;
					}
					jedis.hset("node:" + node + ":conf", var, val);
				}
				
			} 
			
			IQ result = IQ.createResultIQ(reqIQ);
			outQueue.put(result);
		}
		
		private void retract(Element elm) {
			
			String node = elm.attributeValue("node");
			if(node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Node attribute was missing when trying to retract an item.");
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
				
				// TODO We do a post behalf of the sender here to external inbox server.
				
				return;
			} 
			
			Element item = elm.element("item");
			if(item == null) {
				errorQueue.put(ErrorPacketBuilder.badRequest(reqIQ));
				return;
			}
			
			String id = item.attributeValue("id");
			if(id == null) {
				errorQueue.put(ErrorPacketBuilder.badRequest(reqIQ));
				return;
			}
			
			jedis.srem("node:" + node + ":itemset", id);
			jedis.lrem("node:" + node + ":itemlist", 1, id);
			jedis.del("node:" + node + ":item:" + id);
			
			outQueue.put(IQ.createResultIQ(reqIQ));
			
			Message msg = new Message();
			msg.setType(Message.Type.headline);
			msg.setID(UUID.randomUUID().toString());
			Element event = msg.addChildElement("event", JabberPubsubEvent.NAMESPACE_URI);
			Element items = event.addElement("items");
			items.addAttribute("node", node);
			Element retract = items.addElement("retract");
			retract.addAttribute("id", id);
			
			Set<String> externalChannelServerReceivers = new HashSet<String>();
			
			Set<String> subscriberJIDs = jedis.smembers("node:" + node + ":subscribers");
			if(subscriberJIDs.isEmpty()) {
				System.out.println("Weird, there is no subscribers.... (Just retracting here)");
				return;
			}
			
			for (String subscriber : subscriberJIDs) {
				Map<String, String> subscription = jedis.hgetAll("node:" + node + ":subscriber:" + subscriber);
				if(subscription.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER) != null) {
					if(externalChannelServerReceivers.contains(subscription.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER))) {
						continue;
					}
					externalChannelServerReceivers.add(subscription.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER));
					subscriber = subscription.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER);
				} else {
					
					/** TODO
					 *  Add here to deliver only to online local users.
					 */
					
				}
				
				msg.setTo(subscriber);
				outQueue.put(msg.createCopy());
			}
			
		}
		
		private void publish(Element elm, String actor) {
			
			String node = elm.attributeValue("node");
			
			if(node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Node attribute was missing when trying to publish an item.");
				errorQueue.put(ep);
				return;
			}

			String bareJID = null;
			if(actor != null) {
				bareJID = actor;
			} else {
				bareJID = reqIQ.getFrom().toBareJID();
			}
			
			// Let's check if the user is allowed to post to a node.
			if(jedis.hget("node:" + node + ":subscriber:" + bareJID, Subscription.KEY_AFFILIATION) == null) {
				ErrorPacket ep = ErrorPacketBuilder.forbidden(reqIQ);
				ep.setMsg("User posting to a node was not subscribed to it!");
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
				
				// TODO Even if user is not subscribed to a node, it can come here. Fail, no?
				
				// also the external node can end up here? WTF! This is all mixed up!
				
				String channelServer = jedis.hget("node:" + node + ":subscriber:" + reqIQ.getFrom().toBareJID(), Subscription.KEY_EXTERNAL_CHANNEL_SERVER);
				
				// TODO We do a post behalf of the sender here to external inbox server.
				IQ copy = reqIQ.createCopy();
				String id = UUID.randomUUID().toString();
				copy.setID(id);
				copy.setTo(channelServer);
				copy.getChildElement()
				    .addElement("actor", "http://buddycloud.org/v1")
				    .setText(reqIQ.getFrom().toBareJID());
				
				Map<String, String> store = new HashMap<String, String>();
				store.put(State.KEY_STATE, State.STATE_PUBLISH);
				store.put("id", reqIQ.getID());
				store.put("jid", reqIQ.getFrom().toString());
				//jedis.hmset("store:" + id, store);
				StateMachine st = new StateMachine(jedis, outQueue, errorQueue);
				st.store(id, store);
				
				outQueue.put(copy);
				
				return;
			} 
			
			/** 
			 * TODO Check here the sender. 
			 * TODO The sender can be even a local user, or external "actor user".
			 * 
			 * TODO Check also that the user is allowed to publish.
			 */
			
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
			// Verify that the entry is as it should be for buddycloud.
			Validator entryValidator = new Validator(item.element("entry"));
			if(!entryValidator.isValid()) {
				System.out.println("Entry is not valid: '" + entryValidator.getErrorMsg() + "'.");
				ErrorPacket ep = ErrorPacketBuilder.badRequest(reqIQ);
				Element itemRequired = new DOMElement("invalid-payload",
						                              new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
				ep.addCondition(itemRequired);
				ep.setMsg(entryValidator.getErrorMsg());
				return;
			}
			
//			String bareJID = null;
//			if(actor != null) {
//				bareJID = actor;
//			} else {
//				bareJID = reqIQ.getFrom().toBareJID();
//			}
			
			//Element entry = item.element("entry");
			Element entry = entryValidator.createBcCompatible(bareJID, reqIQ.getTo().toBareJID(), node);
			
			String id = entry.element("id").getText();
			String[] idParts = id.split(",");
			id = idParts[2];
			//if(id == null || id.equals("")) {
			//String id = UUID.randomUUID().toString();
			//	item.addAttribute("id", id);
			//}
			
			//Element idElm = entry.element("id");
			//idElm.setText("tag:" + reqIQ.getTo().toBareJID() + "," + node + "," + id);
			
			//String DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
			//SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
			//sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
			
//			Element publishedElm = entry.element("published");
//			if(publishedElm == null) {
//				publishedElm = entry.addElement("published");
//			}
//			publishedElm.setText(sdf.format(new Date()));
//			
//			Element updatedElm = entry.element("updated");
//			if(updatedElm == null) {
//				updatedElm = entry.addElement("updated");
//			}
//			updatedElm.setText(sdf.format(new Date()));
			
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
			
			Message msg = new Message();
			msg.setType(Message.Type.headline);
			msg.setID(id);
			Element event = msg.addChildElement("event", JabberPubsubEvent.NAMESPACE_URI);
			Element items = event.addElement("items");
			items.addAttribute("node", node);
			Element i = items.addElement("item");
			i.addAttribute("id", id);
			i.add(entry.createCopy());
			
			//System.out.println(msg.toXML());
			
			Set<String> externalChannelServerReceivers = new HashSet<String>();
			
			Set<String> subscriberJIDs = jedis.smembers("node:" + node + ":subscribers");
			if(subscriberJIDs.isEmpty()) {
				System.out.println("Weird, there is no subscribers ...");
				return;
			}
			
			for (String subscriber : subscriberJIDs) {
				Map<String, String> subscription = jedis.hgetAll("node:" + node + ":subscriber:" + subscriber);
				if(subscription.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER) != null) {
					if(externalChannelServerReceivers.contains(subscription.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER))) {
						continue;
					}
					externalChannelServerReceivers.add(subscription.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER));
					subscriber = subscription.get(Subscription.KEY_EXTERNAL_CHANNEL_SERVER);
				} else {
					
					/** TODO
					 *  Add here think to deliver only to online local users.
					 */
					
				}
				
				msg.setTo(subscriber);
				outQueue.put(msg.createCopy());
			}
			
			
			/*
			 * TODO
			 * Let's clean if we have too many items on the node.
			 */
		}
		
		private void subscribe(Element elm, String actor) {
			
			String node = elm.attributeValue("node");
			
			if(node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Node attribute was missing when trying to subscribe.");
				errorQueue.put(ep);
				return;
			}
			
//			String jid = elm.attributeValue("jid");
//			JID subsJID = new JID(jid);
//			if(!subsJID.toBareJID().equals(reqIQ.getFrom().toBareJID())) {
//				ErrorPacket ep = ErrorPacketBuilder.badRequest(reqIQ);
//				Element invalidJid = new DOMElement("invalid-jid",
//													new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
//				ep.addCondition(invalidJid);
//				ep.setMsg("JID was not passed as it should. E.g was not same as from in Bare form.");
//				errorQueue.put(ep);
//				return;
//			}
			JID subsJID;
			if(actor != null) {
				subsJID = new JID(actor);
			} else {
				subsJID = reqIQ.getFrom();
			}
			
			// We should allow anyone to subscribe
//			if(subsJID.getNode() != null && !jedis.sismember(JedisKeys.LOCAL_USERS, subsJID.toBareJID())) {
//				errorQueue.put(ErrorPacketBuilder.registrationRequired(reqIQ));
//				return;
//			}
			
			String remoteChannelServer = null;
//			String buddycloudPrefix = "";
//			Namespace namespace = elm.getNamespaceForURI("http://buddycloud.org/v1");
//			if(namespace != null) {
//				buddycloudPrefix = namespace.getPrefix() + ":";
//			}
//			String actor = elm.attributeValue(buddycloudPrefix + "actor");
			
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
			
//			if(!reqIQ.getFrom().toBareJID().equals(subsJID)) {
//				// TODO check that fromJID is allowed to act behalf of subsJID.
//				//
//				// Because we are nice guys, we let everyone to subscribe now :)
//			}
			
			// 7.1.3.3 Node Does Not Exist
			if(!jedis.sismember(JedisKeys.LOCAL_NODES, node) && actor != null) {
				
				/**
				 * Let's discover every time now.
				 */
				
				//if(!jedis.sismember(JedisKeys.REMOTE_NODES, node)) {
					// We need to discover if the channel exists.
				if(node.startsWith("/user/")) {
					String[] splittedNode = node.split("/");
					
					// 0 1    2     3
					// /user/JID/something
					
					JID user = new JID(splittedNode[2]);
					
					IQ discoItemsGet = new IQ();
					String id = UUID.randomUUID().toString();
					discoItemsGet.setType(IQ.Type.get);
					discoItemsGet.setID(id);
					discoItemsGet.setTo(user.getDomain());
					
					discoItemsGet.setChildElement("query", JabberDiscoItems.NAMESPACE_URI);
					
					Map <String, String> store = new HashMap<String, String>();
					store.put(State.KEY_STATE, State.STATE_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT);
					store.put("id", reqIQ.getID());
					store.put("jid", reqIQ.getFrom().toString());
					store.put("node", node);
					
					//jedis.hmset("store:" + id, store);
					StateMachine st = new StateMachine(jedis, outQueue, errorQueue);
					st.store(id, store);
					
					outQueue.put(discoItemsGet);
				} else {
					
					ErrorPacket ep = ErrorPacketBuilder.notAcceptable(reqIQ);
					Element invalidJid = new DOMElement("invalid-nodename",
														new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
					ep.addCondition(invalidJid);
					ep.setMsg("Every node must start with /user/ prefix. Was not the case man!");
					errorQueue.put(ep);
					return;
					
//					JID user = new JID(node);
//					
//					IQ subscribe = new IQ();
//					subscribe.setType(IQ.Type.set);
//					String id = UUID.randomUUID().toString();
//					subscribe.setID(id);
//					subscribe.setTo(user.getDomain());
//					
//					Element pubsub = subscribe.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
//					pubsub.addElement("subscribe")
//					      .addAttribute("node", node)
//					      .addAttribute("jid", reqIQ.getTo().toString());
//					pubsub.addElement("actor", "http://buddycloud.org/v1")
//					      .setText(reqIQ.getFrom().toBareJID());
//					
//					Map <String, String> store = new HashMap<String, String>();
//					store.put(State.KEY_STATE, State.STATE_SUBSCRIBE);
//					store.put("id", reqIQ.getID());
//					store.put("jid", reqIQ.getFrom().toString());
//					store.put("node", node);
//					
//					//jedis.hmset("store:" + id, store);
//					StateMachine st = new StateMachine(jedis, outQueue, errorQueue);
//					st.store(id, store);
//					
//					outQueue.put(subscribe);
				}
				
				return;
			} else if(!jedis.sismember(JedisKeys.LOCAL_NODES, node)) {
				ErrorPacket ep = ErrorPacketBuilder.itemNotFound(reqIQ);
				ep.setMsg("External server tried to subscribe to a node that does not exists!");
				errorQueue.put(ep);
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
	
	private class GetPubSub implements IAction {
		
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
				if(feature.equals("subscriptions")) {
					this.subscriptions(x);
					handled = true;
				} else if(feature.equals("affiliations")) {
					this.affiliations(x);
					handled = true;
				} else if(feature.equals("items")) {
					this.items(x);
					handled = true;
				}
				//break;
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
		
		private void subscriptions(Element elm) {
			
			IQ result = IQ.createResultIQ(reqIQ);
			
			Element pubsub = result.setChildElement(ELEMENT_NAME, NAMESPACE_URI);
			Element subscriptions = pubsub.addElement("subscriptions");
			
			String node = elm.attributeValue("node");
			
			Set<String> subs = jedis.smembers(reqIQ.getFrom().toBareJID() + ":subs");
			if(node == null || subs.contains(node)) {

				if(node != null) {
					subs.clear();
					subs.add(node);
				}
				
				for (String n : subs) {
					subscriptions.addElement("subscription")
								 .addAttribute("node", n)
								 .addAttribute("subscription", jedis.hget("node:" + n + ":subscriber:" + reqIQ.getFrom().toBareJID(), Subscription.KEY_SUBSCRIPTION))
								 .addAttribute("jid", reqIQ.getFrom().toBareJID());
				}
			}
			
			outQueue.put(result);
			
		}
		
		private void affiliations(Element elm) {
			
			IQ result = IQ.createResultIQ(reqIQ);
			
			Element pubsub = result.setChildElement(ELEMENT_NAME, NAMESPACE_URI);
			Element affiliations = pubsub.addElement("affiliations");
			
			String node = elm.attributeValue("node");
			
			Set<String> subs = jedis.smembers(reqIQ.getFrom().toBareJID() + ":subs");
			if(node == null || subs.contains(node)) {

				if(node != null) {
					subs.clear();
					subs.add(node);
				}

				for (String n : subs) {
					affiliations.addElement("affiliation")
								.addAttribute("node", n)
								.addAttribute("affiliation", jedis.hget("node:" + n + ":subscriber:" + reqIQ.getFrom().toBareJID(), Subscription.KEY_AFFILIATION));
				}
			
			} 

			outQueue.put(result);
			
		}
		
		private void items(Element elm) {
			
			String node = elm.attributeValue("node");
			if (node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Tried to fetch node items without passing a node ID.");
				errorQueue.put(ep);
				return;
			}

			// TODO! Add here listing of remote node items too!
			
			List<String> itemsList = jedis.lrange("node:" + node + ":itemlist", 0, -1);
			
			// Let's check if we have RSM
			// implement this too 6.5.7 Requesting the Most Recent Items
			
			Element pubsub = new DOMElement("pubsub",
   											new org.dom4j.Namespace("", NAMESPACE_URI));
			
			Element items = pubsub.addElement("items");
			items.addAttribute("node", node);
			
			for (String itemID : itemsList) {
				
				Element item = items.addElement("item");
				item.addAttribute("id", itemID);
				
				SAXReader xmlReader = new SAXReader();
				Element entry = null;
				try {
					entry = xmlReader.read(new StringReader(jedis.get("node:" + node + ":item:" + itemID))).getRootElement();
					item.add(entry);
				} catch (DocumentException e) {
					System.out.println("Something went very wrong here.");
				}
				
			}
			
			IQ result = IQ.createResultIQ(reqIQ);
			result.setChildElement(pubsub);
			
			outQueue.put(result);
		}
	}
	
	private class ResultPubSub implements IAction {

		public static final String ELEMENT_NAME = "pubsub";

		@Override
		public void process() {
			
			StateMachine sm = new StateMachine(jedis, outQueue, errorQueue);
			sm.ingest(reqIQ);
			
		}
	}
	
	private class ErrorPubSub implements IAction {
		
		public static final String ELEMENT_NAME = "pubsub";

		@Override
		public void process() {
			
			StateMachine sm = new StateMachine(jedis, outQueue, errorQueue);
			sm.ingestError(reqIQ);
			
		}
	}
}
