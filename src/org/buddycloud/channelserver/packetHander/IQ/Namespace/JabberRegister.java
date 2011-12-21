package org.buddycloud.channelserver.packetHander.IQ.Namespace;

import java.util.Properties;

import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.queue.AOutQueue;
import org.xmpp.packet.IQ;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.IQ.Type;

import redis.clients.jedis.Jedis;

public final class JabberRegister extends AbstractNamespace {

	public static final String NAMESPACE_URI = "jabber:iq:register";
	
	public JabberRegister(AOutQueue outQueue, Properties conf, DataStore dataStore) {
		
		super(outQueue, conf, dataStore);
		setProcessors.put(Query.ELEMENT_NAME, new Query());	
	
	}

	private class Query implements IAction {

		public static final String ELEMENT_NAME = "query";
		
		@Override
		public void process() {
			
		    String domain  = reqIQ.getFrom().getDomain();
		    if(!domain.equals(conf.getProperty("server.domain"))) {
		        
		        // Request is coming from different domain than the
		        // component is using. We will not allow this because
		        // "buddycloud federation" cannot work for that.
		        
		        IQ reply = IQ.createResultIQ(reqIQ);
		        reply.setType(Type.error);
		        reply.setChildElement(reqIQ.getChildElement().createCopy());
	            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.not_allowed, 
	                                             org.xmpp.packet.PacketError.Type.cancel);
	            reply.setError(pe);
	            
	            outQueue.put(reply);
		        return;
		    }
		    
			String bareJID = reqIQ.getFrom().toBareJID();
			if(dataStore.addLocalUser(bareJID) == 0) {
			    
			    // User is already registered.
			    
			    IQ reply = IQ.createResultIQ(reqIQ);
                reply.setType(Type.error);
                reply.setChildElement(reqIQ.getChildElement().createCopy());
                PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.conflict, 
                                                 org.xmpp.packet.PacketError.Type.cancel);
                reply.setError(pe);
                
                outQueue.put(reply);
                return;
			}
			
			//dataStore.createUserPostsNode(reqIQ.getFrom().toBareJID());
			dataStore.createUserNodes(reqIQ.getFrom().toBareJID());
			
			IQ result = IQ.createResultIQ(reqIQ);
			outQueue.put(result);
		}
		
	}
}
