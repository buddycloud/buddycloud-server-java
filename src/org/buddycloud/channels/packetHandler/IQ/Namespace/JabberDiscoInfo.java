package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.State;
import org.buddycloud.channels.statefull.StateMachine;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
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
			
			/**
			 * THIS STARTS TO BE A TERRIBLE TERRIBLE HACK.
			 * 
			 * BUT IT SHOULD. THIS IS A PROOF OF CONCEPT MOCK UP.
			 */
			
			IQ result = IQ.createResultIQ(reqIQ);
			
			Element elm = reqIQ.getChildElement();
			String node = elm.attributeValue("node");
			
			if(node == null) {
				Element query = result.setChildElement(ELEMENT_NAME, JabberDiscoInfo.NAMESPACE_URI);
				query.addElement("identity")
					 .addAttribute("category", "pubsub")
					 .addAttribute("type", "channels")
					 .addAttribute("name", "Koski's buddycloud channel server!");
				
				query.addElement("feature")
					 .addAttribute("var", "http://jabber.org/protocol/disco#info");
			} else {
				
				if(!jedis.sismember(JedisKeys.LOCAL_NODES, node)) {
					
					if("".equals(reqIQ.getFrom().getNode()) || reqIQ.getFrom().getNode() == null) { 
						// This means that the info is requested from another channel server
						errorQueue.put(ErrorPacketBuilder.itemNotFound(reqIQ));
						return;
					}
					
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
						store.put(State.KEY_STATE, State.STATE_DISCOINFO_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT);
						store.put("id", reqIQ.getID());
						store.put("jid", reqIQ.getFrom().toString());
						store.put("node", node);
						
						jedis.hmset("store:" + id, store);
						
						outQueue.put(discoItemsGet);
					} else {
						JID user = new JID(node);
						
						IQ discoItemsGet = new IQ();
						String id = UUID.randomUUID().toString();
						discoItemsGet.setType(IQ.Type.get);
						discoItemsGet.setID(id);
						discoItemsGet.setTo(user.getDomain());
						
						Element query = discoItemsGet.setChildElement("query", JabberDiscoInfo.NAMESPACE_URI);
		                query.addAttribute("node", node);
		                query.addElement("actor", "http://buddycloud.org/v1")
					         .setText(reqIQ.getFrom().toBareJID());
		                
						Map <String, String> store = new HashMap<String, String>();
						store.put(State.KEY_STATE, State.STATE_DISCOINFO);
						store.put("id", reqIQ.getID());
						store.put("jid", reqIQ.getFrom().toString());
						store.put("node", node);
						
						jedis.hmset("store:" + id, store);
						
						outQueue.put(discoItemsGet);
					}
					
					return;
				}
				
				Map<String, String> conf = jedis.hgetAll("node:" + node + ":conf");
				
				elm.addElement("identity").addAttribute("category", "pubsub")
				                          .addAttribute("type", "leaf");
				
				elm.addElement("feature", "http://jabber.org/protocol/pubsub");
				
				Element x = elm.addElement("x", "jabber:x:data");
				x.addAttribute("type", "result");
				
				Element field = x.addElement("field");
				field.addAttribute("var", "FORM_TYPE");
				field.addAttribute("type", "hidden");
				field.addElement("value").setText("http://jabber.org/protocol/pubsub#meta-data");
				
				for (String key : conf.keySet()) {
					field = x.addElement("field");
					field.addAttribute("var", key);
					field.addAttribute("type", "text-single");
					field.addElement("value").setText(conf.get(key));
				}
				
				result.setChildElement(elm.createCopy());
			}
			
			outQueue.put(result);
			
		}
	}
	
	private class ResultQuery implements IAction {

		public static final String ELEMENT_NAME = "query";
		
		@Override
		public void process() {
			
			StateMachine sm = new StateMachine(jedis, outQueue, errorQueue);
			sm.ingest(reqIQ);
			
		}
		
	}
}
