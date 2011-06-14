package org.buddycloud.channels.packetHandler.IQ.Namespace;

import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;

import redis.clients.jedis.Jedis;

public class JabberPubsubEvent extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/pubsub#event";
	
	public JabberPubsubEvent(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		super(outQueue, errorQueue, jedis);
		//setProcessors.put(SetPubSub.ELEMENT_NAME, new SetPubSub());
		
	}
	
}
