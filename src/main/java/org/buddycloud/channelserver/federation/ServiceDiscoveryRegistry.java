package org.buddycloud.channelserver.federation;

import java.util.Collection;
import java.util.HashMap;

import org.buddycloud.channelserver.connection.iq.IQRequestProcessor;
import org.buddycloud.channelserver.federation.requests.disco.DiscoInfoIQRequest;
import org.buddycloud.channelserver.federation.requests.disco.DiscoItemsIQRequest;
import org.buddycloud.channelserver.federation.requests.disco.DiscoInfoIQRequest.Identity;
import org.xmpp.packet.JID;

public class ServiceDiscoveryRegistry {
	public interface JIDDiscoveryHandler {
		void onSuccess(JID jid);
		void onError();
	}
	
	private final IQRequestProcessor iqRequestProcessor;
	
	private final HashMap<String,JID> channelServers;
	
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
