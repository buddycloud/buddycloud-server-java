package org.buddycloud.channelserver.channel;

import java.util.ArrayList;
import java.util.Collection;

import org.buddycloud.channelserver.connection.iq.IQRequestProcessor;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.federation.requests.pubsub.GetUserAffiliationsProcessor;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.xmpp.packet.JID;
import org.xmpp.packet.PacketError;

public class AsyncChannelManager {
	private final ChannelManager delegate;
	private final IQRequestProcessor iqRequestProcessor;

	public AsyncChannelManager(ChannelManager delgate) {
		this.delegate = delgate;
	}

	void getUserAffiliations(final GetUserAffiliationsProcessor.Handler handler, JID user) throws NodeStoreException {
		Collection<NodeAffiliation> affiliations = delegate.getUserAffiliations(user);
		
		if(!affiliations.isEmpty() || delegate.isLocalJID(user)) {
			handler.onSuccess(affiliations);
		}
		
		GetUserAffiliationsProcessor request = new GetUserAffiliationsProcessor(user);
		
		final Thread thread = Thread.currentThread();
		
		final ArrayList<Collection<NodeAffiliation>> result = new ArrayList<Collection<NodeAffiliation>>(1);
		final ArrayList<PacketError> error = new ArrayList<PacketError>(1);
		
		request.send(iqThing, handler);
	}
}
