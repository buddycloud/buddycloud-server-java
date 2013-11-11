package org.buddycloud.channelserver.utils;

import org.xmpp.packet.JID;

public class NullJid extends JID {

	private static final long serialVersionUID = 267645925029196121L;

	public NullJid() {
		super("null");
	}

	public String toBareJID() {
    	return null;
    }
	
	public String toFullJID() {
		return null;
	}
	
	public String getDomain() {
		return null;
	}
}
