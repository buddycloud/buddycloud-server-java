package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.pubsub.Subscription;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;

import redis.clients.jedis.Jedis;

public class JabberPubsubOwner extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/pubsub#owner";
	
	public JabberPubsubOwner(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		super(outQueue, errorQueue, jedis);
		setProcessors.put(SetPubSub.ELEMENT_NAME, new SetPubSub());
		getProcessors.put(GetPubSub.ELEMENT_NAME, new GetPubSub());
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
				if(feature.equals("configure")) {
					this.configure(x);
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
		
		private void configure(Element elm) {
			
			String node = elm.attributeValue("node");
			if(node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Node attribute was missing when trying to configure it.");
				errorQueue.put(ep);
				return;
			}

			if(!jedis.sismember(JedisKeys.LOCAL_NODES, node)) {
				errorQueue.put(ErrorPacketBuilder.itemNotFound(reqIQ));
				return;
			}

			// Let's check that the owner is always an owner.
			if(!reqIQ.getFrom().toBareJID().equals(jedis.hget("node:" + node + ":conf", "pubsub#owner"))) {
				errorQueue.put(ErrorPacketBuilder.forbidden(reqIQ));
				return;
			}
			
			Set <String> configurableValues = new HashSet<String>();
			configurableValues.add("pubsub#title");
			configurableValues.add("pubsub#description");
			
			Element x = elm.element("x");
			
			if(x == null) {
				ErrorPacket ep = ErrorPacketBuilder.badRequest(reqIQ);
				ep.setMsg("Bad configuration payload. No x -element found.");
				errorQueue.put(ep);
				return;
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
				
				if(!configurableValues.contains(var)) {
					continue;
				}
				
				String val = value.getTextTrim();
				
				jedis.hset("node:" + node + ":conf", var, val);
			}
			
			IQ result = IQ.createResultIQ(reqIQ);
			outQueue.put(result);
			
			if( !"1".equals(jedis.hget("node:" + node + ":conf", "pubsub#notify_config")) ) {
				return;
			}
			
			// The crazy mockup delivery.
			
			Message msg = new Message();
			msg.setType(Message.Type.headline);
			msg.setID(UUID.randomUUID().toString());
			Element event = msg.addChildElement("event", 
												JabberPubsubEvent.NAMESPACE_URI);
			
			Element configuration = event.addElement("confguration");
			configuration.addAttribute("node", node);
			
			x = configuration.addElement("x", "jabber:x:data");
			x.addAttribute("type", "result");
			
			Element field = x.addElement("field");
			field.addAttribute("var", "FORM_TYPE");
			field.addAttribute("type", "hidden");
			field.addElement("value").setText("http://jabber.org/protocol/pubsub#node_config");
			
			Map<String, String> conf = jedis.hgetAll("node:" + node + ":conf");
			for (String key : conf.keySet()) {
				field = x.addElement("field");
				field.addAttribute("var", key);
				field.addElement("value").setText(conf.get(key));
			}
			
			Set<String> externalChannelServerReceivers = new HashSet<String>();
			
			Set<String> subscriberJIDs = jedis.smembers("node:" + node + ":subscribers");
			if(subscriberJIDs.isEmpty()) {
				System.out.println("Weird, there is no subscribers....");
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
			
			if(node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Node attribute was missing when trying to fetch subscriptions.");
				errorQueue.put(ep);
				return;
			}
			
			if(!jedis.sismember(JedisKeys.LOCAL_NODES, node)) {
				ErrorPacket ep = ErrorPacketBuilder.itemNotFound(reqIQ);
				ep.setMsg("Tried to fetch subscriptions of a node that does not exists!");
				errorQueue.put(ep);
				return;
			}
			
			subscriptions.addAttribute("node", node);
			
			Set<String> subscriberJIDs = jedis.smembers("node:" + node + ":subscribers");
			for (String subscriberJID : subscriberJIDs) {
				subscriptions.addElement("subscription")
				 			 .addAttribute("subscription", jedis.hget("node:" + node + ":subscriber:" + subscriberJID, Subscription.KEY_SUBSCRIPTION))
				 			 .addAttribute("jid", subscriberJID);
			}
			
			outQueue.put(result);
			
		}
		
		private void affiliations(Element elm) {
			
			IQ result = IQ.createResultIQ(reqIQ);
			
			Element pubsub = result.setChildElement(ELEMENT_NAME, NAMESPACE_URI);
			Element affiliations = pubsub.addElement("affiliations");
			
			String node = elm.attributeValue("node");
			
			if(node == null || node.equals("")) {
				ErrorPacket ep = ErrorPacketBuilder.nodeIdRequired(reqIQ);
				ep.setMsg("Node attribute was missing when trying to fetch affiliations.");
				errorQueue.put(ep);
				return;
			}
			
			if(!jedis.sismember(JedisKeys.LOCAL_NODES, node)) {
				ErrorPacket ep = ErrorPacketBuilder.itemNotFound(reqIQ);
				ep.setMsg("Tried to fetch affiliations of a node that does not exists!");
				errorQueue.put(ep);
				return;
			}
			
			affiliations.addAttribute("node", node);
			
			Set<String> subscriberJIDs = jedis.smembers("node:" + node + ":subscribers");
			for (String subscriberJID : subscriberJIDs) {
				affiliations.addElement("affiliation")
				 			.addAttribute("affiliation", jedis.hget("node:" + node + ":subscriber:" + subscriberJID, Subscription.KEY_AFFILIATION))
				 			.addAttribute("jid", subscriberJID);
			}
			
			outQueue.put(result);
			
		}
	}
}
