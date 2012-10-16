package org.buddycloud.channelserver.federation;

import org.buddycloud.channelserver.connection.iq.IQRequest;
import org.buddycloud.channelserver.connection.iq.IQRequestProcessor;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class FederatedRequestService {
	private final IQRequestProcessor iqRequestProcessor;
	private final ServiceDiscoveryRegistry discovery;

	public FederatedRequestService(final IQRequestProcessor iqRequestProcessor, final ServiceDiscoveryRegistry discovery) {
		this.iqRequestProcessor = iqRequestProcessor;
		this.discovery = discovery;
	}

	public void sendIQRequestToChannelServer(final IQRequest request, final JID to) {
		IQ iq = request.getRequest();
		
		
	}
	
}
