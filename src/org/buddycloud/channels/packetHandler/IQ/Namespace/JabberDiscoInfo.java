package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.Map;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.StateMachine;
import org.dom4j.Element;
import org.xmpp.packet.IQ;

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
					errorQueue.put(ErrorPacketBuilder.itemNotFound(reqIQ));
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
