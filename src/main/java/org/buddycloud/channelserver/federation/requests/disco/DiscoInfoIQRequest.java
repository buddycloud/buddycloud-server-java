package org.buddycloud.channelserver.federation.requests.disco;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.buddycloud.channelserver.connection.iq.IQRequest;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo.JabberDiscoInfo;
import org.dom4j.Element;
import org.dom4j.QName;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class DiscoInfoIQRequest implements IQRequest {
	public interface Handler {
		void onSuccess(Collection<Identity> result);
	}
	
	public class Identity {
		private String category;
		private String type;

		public Identity(String category, String type) {
			this.category = category;
			this.type = type;
		}

		public String getCategory() {
			return category;
		}

		public String getType() {
			return type;
		}
	}
	
	private Handler handler;
	private JID jid;
	
	public DiscoInfoIQRequest(final JID jid, final Handler handler) {
		this.jid = jid;
		this.handler = handler;
	}
	
	@Override
	public IQ getRequest() {
		IQ iq = new IQ();
		
		iq.setType(IQ.Type.get);
		iq.setTo(jid);
		iq.setFrom("channels.myserver");
		iq.setChildElement("query",
				JabberDiscoInfo.NAMESPACE_URI);
		
		return iq;
	}

	@Override
	public void onResult(IQ response) {
		Element queryEl = response.getChildElement();
		
		@SuppressWarnings("unchecked")
		List<Element> itemEls = queryEl.elements(QName.get("identity", JabberDiscoInfo.NAMESPACE));
		
		ArrayList<Identity> items = new ArrayList<Identity>(itemEls.size());
		
		for(Element itemEl : itemEls) {
			items.add(new Identity(itemEl.attributeValue("category"), itemEl.attributeValue("type")));
		}
		
		handler.onSuccess(items);
	}

	@Override
	public void onError(IQ error) {
		// TODO Auto-generated method stub
	}
}
