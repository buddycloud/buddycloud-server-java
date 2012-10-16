package org.buddycloud.channelserver.federation.requests.pubsub;

import java.util.Collection;

import org.buddycloud.channelserver.connection.iq.IQRequest;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo.JabberDiscoInfo;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.Buddycloud;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.queue.OutQueueConsumer;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class GetUserAffiliationsProcessor {
	public interface Handler {
		public void onSuccess(Collection<NodeAffiliation> affiliations);
		public void onError(PacketError packetError);
	}
	
	public class Request implements IQRequest {

		public IQ getRequest(JID jid) {

			return null;
		}

		@Override
		public void onResult(IQ response) {
			
		}

		@Override
		public void onError(IQ error) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public IQ getRequest() {
			// TODO Auto-generated method stub
			return null;
		}
		
	}
	
	private JID user;

	public GetUserAffiliationsProcessor(JID user) {
		this.user = user;
	}

	public IQ getRequest() {
		Packet packet = new IQ();

	    Element iq = packet.getElement();
		Element pubsub = iq.addElement("pubsub",
				JabberPubsub.NAMESPACE_URI);
	    Element affiliations = pubsub.addElement("affiliations");
	    affiliations.addAttribute("jid", user.toBareJID());
		Element actor = pubsub.addElement("actor");
		actor.addAttribute("jid", user.toBareJID());
		actor.addNamespace("", Buddycloud.NAMESPACE);
		return (IQ) packet;
	}

	public void onResult(IQ response) {
		// TODO Auto-generated method stub

	}

	public void onError(IQ error) {
		// TODO Auto-generated method stub

	}

}
