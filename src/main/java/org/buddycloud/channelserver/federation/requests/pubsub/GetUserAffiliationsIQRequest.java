package org.buddycloud.channelserver.federation.requests.pubsub;

import java.util.Collection;

import org.buddycloud.channelserver.connection.iq.IQRequest;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.PacketError;

public class GetUserAffiliationsProcessor {
	public interface Handler {
		public void onSuccess(Collection<NodeAffiliation> affiliations);
		public void onError(PacketError packetError);
	}
	
	public class Request implements IQRequest {

		@Override
		public IQ getRequest() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public void onResult(IQ response) {
			
		}

		@Override
		public void onError(IQ error) {
			// TODO Auto-generated method stub
			
		}
		
	}
	
	private JID user;

	public GetUserAffiliationsProcessor(JID user) {
		this.user = user;
	}

	@Override
	public IQ getRequest() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void onResult(IQ response) {
		// TODO Auto-generated method stub

	}

	@Override
	public void onError(IQ error) {
		// TODO Auto-generated method stub

	}

}
