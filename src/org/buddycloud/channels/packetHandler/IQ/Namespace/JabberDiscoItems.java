package org.buddycloud.channels.packetHandler.IQ.Namespace;

import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.buddycloud.channels.statefull.StateMachine;

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
			
			StateMachine sm = new StateMachine(jedis, outQueue, errorQueue);
			sm.ingest(reqIQ);
			
		}
		
	}
	
}
