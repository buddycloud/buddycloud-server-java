package org.buddycloud.channelserver.federation.requests.disco;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.buddycloud.channelserver.connection.iq.IQRequest;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.discoitems.JabberDiscoItems;
import org.dom4j.Element;
import org.dom4j.QName;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class DiscoItemsIQRequest implements IQRequest {
	public interface Handler {
		void onSuccess(Collection<JID> result);
	}
	
	private Handler handler;
	private JID jid;
	
	public DiscoItemsIQRequest(final JID jid, final Handler handler) {
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
				JabberDiscoItems.NAMESPACE_URI);
		
		return iq;
	}

	@Override
	public void onResult(IQ response) {
		ArrayList<JID> items = new ArrayList<JID>();
		
		Element queryEl = response.getChildElement();
		
		@SuppressWarnings("unchecked")
		List<Element> itemEls = queryEl.elements(QName.get("item", JabberDiscoItems.NAMESPACE));
		
		for(Element itemEl : itemEls) {
			items.add(new JID(itemEl.attributeValue("jid")));
		}
		
		handler.onSuccess(items);
	}

	@Override
	public void onError(IQ error) {
		// TODO Auto-generated method stub
	}
}
