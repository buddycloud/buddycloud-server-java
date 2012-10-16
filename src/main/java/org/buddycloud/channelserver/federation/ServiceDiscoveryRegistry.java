package org.buddycloud.channelserver.federation;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.buddycloud.channelserver.connection.iq.IQRequestProcessor;
import org.buddycloud.channelserver.federation.requests.disco.DiscoInfoIQRequest;
import org.buddycloud.channelserver.federation.requests.disco.DiscoItemsIQRequest;
import org.buddycloud.channelserver.federation.requests.disco.DiscoInfoIQRequest.Identity;
import org.xmpp.packet.JID;

public class ServiceDiscoveryRegistry {
	public interface JIDDiscoveryHandler {
		void onSuccess(JID jid);
		void onError(Throwable t);
	}
	
	private final IQRequestProcessor iqRequestProcessor;
	
	private final Map<String,JID> channelServers;
	
	public ServiceDiscoveryRegistry(final IQRequestProcessor iqRequestProcessor) {
		this.iqRequestProcessor = iqRequestProcessor;
		channelServers = new HashMap<String,JID>();
	}
	
	/**
	 * Discovers the relevant channel server JID for a remote JID
	 * @param handler the response handler which will be called 
	 * @param remoteJID
	 */
	public void discoverChannelServerJID(final JID remoteJID, final JIDDiscoveryHandler handler) {
		// TODO Ensure that we don't send the same request twice at the same time
		
		// We first discover the items on the JID's domain
		final String remoteDomain = remoteJID.getDomain();
		
		final JID cachedJID = channelServers.get(remoteDomain);
		
		if(cachedJID != null) {
			handler.onSuccess(cachedJID);
			return;
		}
		
		DiscoItemsIQRequest itemsRequest = new DiscoItemsIQRequest(new JID(remoteDomain), new DiscoItemsIQRequest.Handler() {
			
			@Override
			public void onSuccess(final Collection<JID> result) {
				// Then for each item we do an info query until we find the appropriate identity
				for(final JID jid : result) {
					// We will recheck the map each iteration in case another thread has added it a result in the meantime.
					// This is less expensive than potentially sending more disco#info requests than we strictly need to.
					final JID cachedJID = channelServers.get(remoteDomain);
					
					if(cachedJID != null) {
						handler.onSuccess(cachedJID);
						return;
					}

					DiscoInfoIQRequest infoRequest = new DiscoInfoIQRequest(jid, new DiscoInfoIQRequest.Handler() {
						
						@Override
						public void onSuccess(final Collection<Identity> result) {
							for(Identity identity : result) {
								if(identity.getCategory().equals("pubsub") && identity.getCategory().equals("channels")) {
									channelServers.put(remoteDomain, jid);
								}
							}
						}
					});
					
					iqRequestProcessor.processRequest(infoRequest);
				}
			}
		});
		
		iqRequestProcessor.processRequest(itemsRequest);
	}
}
