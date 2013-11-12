package org.buddycloud.channelserver.utils.matcher;

import org.mockito.internal.matchers.Equals;
import org.xmpp.packet.JID;

public class BareJidMatcher extends Equals {

	public BareJidMatcher(Object wanted) {
		super(wanted);
	}

	public boolean matches(Object jid) {
		try {
			JID wanted = (JID) getWanted();
			JID received = (JID) jid;
			return received.toBareJID().equals(wanted.toBareJID());	
		} catch (NullPointerException e) {
			return false;
		}
	}
}
