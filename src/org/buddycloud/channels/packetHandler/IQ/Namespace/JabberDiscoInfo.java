package org.buddycloud.channels.packetHandler.IQ.Namespace;

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
			
			StateMachine sm = new StateMachine(jedis, outQueue, errorQueue);
			sm.ingest(reqIQ);
			
		}
		
	}
}
