package org.buddycloud.channels.packet;

import java.util.ArrayList;
import java.util.Collection;

import org.dom4j.Element;
import org.xmpp.packet.IQ;

public class ErrorPacket {

	public static final String NS_XMPP_STANZAS = "urn:ietf:params:xml:ns:xmpp-stanzas";
	public static final String NS_PUBSUB_ERROR = "http://jabber.org/protocol/pubsub#errors";
	
	private String type;
	
	private Collection<Element> conditions = new ArrayList<Element>();
	
	private IQ originalReq;
	
	private String msg = "";
	
	public ErrorPacket(IQ originalReq, String type, Element condition) {
		this.originalReq = originalReq;
		this.type = type;
		if(condition != null) {
			this.conditions.add(condition);
		}
	}
	
	public String getMsg() {
		return this.msg;
	}
	
	public void setMsg(String msg) {
		this.msg = msg;
	}
	
	public String getType() {
		return this.type;
	}
	
	public IQ getOriginalIQ() {
		return this.originalReq;
	}
	
	public Collection<Element> getConditions() {
		return this.conditions;
	}
	
	public void addCondition(Element elm) {
		this.conditions.add(elm);
	}
	
}
