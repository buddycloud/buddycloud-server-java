package org.buddycloud.channelserver.federation.requests.pubsub;

import java.util.Collection;

import org.buddycloud.channelserver.connection.iq.IQRequest;
import org.buddycloud.channelserver.connection.iq.IQRequestProcessor;
import org.buddycloud.channelserver.federation.AsyncCall;
import org.buddycloud.channelserver.federation.ServiceDiscoveryRegistry;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class AddUserSubscriptionRequest implements AsyncCall<Collection<NodeAffiliation>> {
	private final IQRequestProcessor iqRequestProcessor;
	private final ServiceDiscoveryRegistry discovery;
	
	private final NodeSubscription subscription;

	public AddUserSubscriptionRequest(final IQRequestProcessor iqRequestProcessor, final ServiceDiscoveryRegistry discovery, final NodeSubscription subscription) {
		this.subscription = subscription;
		this.iqRequestProcessor = iqRequestProcessor;
		this.discovery = discovery;
	}

	@Override
	public void call(final ResultHandler<Collection<NodeAffiliation>> handler) {
		discovery.discoverChannelServerJID(subscription.getListener(), new ServiceDiscoveryRegistry.JIDDiscoveryHandler() {
			
			@Override
			public void onSuccess(JID jid) {
				sendRequest(jid);
			}
			
			@Override
			public void onError(Throwable t) {
				handler.onError(t);
			}
		});
	}
	
	private void sendRequest(final JID remoteServer) {
		IQRequest request = new IQRequest() {
			
			@Override
			public void onResult(IQ response) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onError(IQ error) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public IQ getRequest() {
				IQ iq = new IQ(IQ.Type.get);

				Element el = iq.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
				
				el.addElement("affiliations");
				
				return iq;
			}
		};
		
		iqRequestProcessor.processRequest(request);
	}
}