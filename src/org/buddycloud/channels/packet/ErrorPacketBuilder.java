package org.buddycloud.channels.packet;

import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;

public class ErrorPacketBuilder {

	public static ErrorPacket itemNotFound(IQ reqIQ) {
		Element itemNotFound = new DOMElement("item-not-found",
											  new org.dom4j.Namespace("", "urn:ietf:params:xml:ns:xmpp-stanzas"));
		return new ErrorPacket(reqIQ, "cancel", itemNotFound);
	}
	
	public static ErrorPacket featureNotImplemented(IQ reqIQ) {
		Element featureNotImplemented = new DOMElement("feature-not-implemented",
													   new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		return new ErrorPacket(reqIQ, "cancel", featureNotImplemented);
	}
	
	public static ErrorPacket internalServerError(IQ reqIQ) {
		Element internalServerError = new DOMElement("internal-server-error",
				   									 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		return new ErrorPacket(reqIQ, "wait", internalServerError);
	}
	
	public static ErrorPacket badRequest(IQ reqIQ) {
		Element internalServerError = new DOMElement("bad-request",
				   									 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		return new ErrorPacket(reqIQ, "modify", internalServerError);
	}	
	
	public static ErrorPacket unexpectedRequest(IQ reqIQ) {
		Element unexpectedRequest = new DOMElement("unexpected-request",
				   									 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		return new ErrorPacket(reqIQ, "cancel", unexpectedRequest);
	}
	
	public static ErrorPacket notAcceptable(IQ reqIQ) {
		Element unexpectedRequest = new DOMElement("not-acceptable",
				   									 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		return new ErrorPacket(reqIQ, "modify", unexpectedRequest);
	}
	
	public static ErrorPacket conflict(IQ reqIQ) {
		Element internalServerError = new DOMElement("conflict",
				   									 new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		return new ErrorPacket(reqIQ, "cancel", internalServerError);
	}
	
	
	public static ErrorPacket nodeIdRequired(IQ reqIQ) {
		Element badRequest = new DOMElement("bad-request",
						   					new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		
		Element nodeIdRequired = new DOMElement("nodeid-required",
												new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
		
		ErrorPacket ep = new ErrorPacket(reqIQ, "modify", null);
		ep.addCondition(badRequest);
		ep.addCondition(nodeIdRequired);
		return ep;
	}
	
	public static ErrorPacket registrationRequired(IQ reqIQ) {
		Element registrationRequired = new DOMElement("registration-required",
					 								  new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));
		return new ErrorPacket(reqIQ, "auth", registrationRequired);
	}
	
	public static ErrorPacket tooManySubscriptions(IQ reqIQ) {
		Element badRequest = new DOMElement("policy-violation",
											new org.dom4j.Namespace("", ErrorPacket.NS_XMPP_STANZAS));

		Element nodeIdRequired = new DOMElement("too-many-subscriptions",
												new org.dom4j.Namespace("", ErrorPacket.NS_PUBSUB_ERROR));
		
		ErrorPacket ep = new ErrorPacket(reqIQ, "wait", null);
		ep.addCondition(badRequest);
		ep.addCondition(nodeIdRequired);
		return ep;
}
}
