package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.util.Set;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.StateMachine;
import org.dom4j.Element;
import org.xmpp.packet.IQ;

import redis.clients.jedis.Jedis;

public class JabberDiscoItems extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/disco#items";
	
	public JabberDiscoItems(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		super(outQueue, errorQueue, jedis);
		resultProcessors.put(ResultQuery.ELEMENT_NAME, new ResultQuery());
		getProcessors.put(GetQuery.ELEMENT_NAME, new GetQuery());
		
	}

	private class GetQuery implements IAction {

		public static final String ELEMENT_NAME = "query";
		
		@Override
		public void process() {

			IQ result  = IQ.createResultIQ(reqIQ);
			
			Element elm = reqIQ.getChildElement();
			String node = elm.attributeValue("node");
			if(node == null || node.equals("")) {
				
				Set<String> nodes = jedis.smembers(JedisKeys.LOCAL_NODES);
				
				Element query = result.setChildElement("query", NAMESPACE_URI);
				
				for (String nodename : nodes) {
					
					String name = jedis.hget("node:" + nodename + ":conf", "pubsub#title");
					
					query.addElement("item")
						 .addAttribute("jid", reqIQ.getTo().toBareJID())
						 .addAttribute("node", nodename)
						 .addAttribute("name", name);
				}
				
			} else {
			
				if(!jedis.sismember(JedisKeys.LOCAL_NODES, node)) {
//					// 7.1.3.3 Node Does Not Exist
//					// TODO, this is not a good place here. It is possible that we want to do 
//					// discovery here and send the packet. All open nodes for everyone can exists.
					errorQueue.put(ErrorPacketBuilder.itemNotFound(reqIQ));
					return;
				}
				
				Element query = result.setChildElement("query", NAMESPACE_URI);
				query.addAttribute("node", node);
				
				Set<String> items = jedis.smembers("node:" + node + ":itemset");
				
				for (String itemID : items) {
					query.addElement("item")
					     .addAttribute("jid", reqIQ.getTo().toBareJID())
					     .addAttribute("name", itemID);
				}
				
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
