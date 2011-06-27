package org.buddycloud.channels.packetHandler.IQ.Namespace;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.packet.ErrorPacket;
import org.buddycloud.channels.packet.ErrorPacketBuilder;
import org.buddycloud.channels.pubsub.Subscription;
import org.buddycloud.channels.pubsub.subscription.Type;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;
import org.xmpp.packet.IQ;

import redis.clients.jedis.Jedis;

public final class JabberRegister extends AbstractNamespace {

	public static final String NAMESPACE_URI = "jabber:iq:register";
	
	public JabberRegister(OutQueue outQueue, ErrorQueue errorQueue, Jedis jedis) {
		
		super(outQueue, errorQueue, jedis);
		setProcessors.put(Query.ELEMENT_NAME, new Query());	
	
	}

	private class Query implements IAction {

		public static final String ELEMENT_NAME = "query";
		
		@Override
		public void process() {
			
			String bareJID = reqIQ.getFrom().toBareJID();
			
			if(jedis.sadd(JedisKeys.LOCAL_USERS, reqIQ.getFrom().toBareJID()) == 0) {
				ErrorPacket ep = ErrorPacketBuilder.conflict(reqIQ);
				ep.setMsg("User tried to register even he was already registered.");
				errorQueue.put(ep);
				return;
			}
			
			String node = "/user/" + reqIQ.getFrom().toBareJID() + "/status";
			
			Subscription sub = new Subscription(Type.subscribed,
												org.buddycloud.channels.pubsub.affiliation.Type.owner,
												null);

			jedis.sadd("node:" + node + ":subscribers", reqIQ.getFrom().toBareJID());
			jedis.hmset("node:" + node + ":subscriber:" + reqIQ.getFrom().toBareJID(), sub.getAsMap());
			jedis.set("node:" + node + ":owner", reqIQ.getFrom().toBareJID());
			
			jedis.sadd(JedisKeys.LOCAL_NODES, "/user/" + reqIQ.getFrom().toBareJID() + "/status");
			
			String DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
			SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
			sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
			
			Map <String, String> conf = new HashMap<String, String>();
			conf.put("pubsub#type", "http://www.w3.org/2005/Atom");
			conf.put("pubsub#title", bareJID + " status.");
			conf.put("pubsub#description", bareJID + " status feed. This is a bit like his \"twitter timeline\".");
			conf.put("pubsub#publish_model", "publishers");
			conf.put("pubsub#access_model", "open");
			conf.put("pubsub#creation_date", sdf.format(new Date()));
			conf.put("pubsub#owner", reqIQ.getFrom().toBareJID());
			conf.put("pubsub#default_affiliation", org.buddycloud.channels.pubsub.affiliation.Type.member.toString());
			conf.put("pubsub#num_subscribers", "1");
			conf.put("pubsub#notify_config", "1");
			
			jedis.hmset("node:/user/" + reqIQ.getFrom().toBareJID() + "/status:conf", conf);
			
			jedis.sadd(reqIQ.getFrom().toBareJID() + ":subs", node);
			
			IQ result = IQ.createResultIQ(reqIQ);
			outQueue.put(result);
		}
		
	}
}
