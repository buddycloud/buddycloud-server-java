package org.buddycloud.channelserver.connection;

import org.buddycloud.channelserver.connection.iq.IQRequest;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

/**
 * Very simple interface to allow sending packets through an XMPP connection
 */
public interface XMPPConnection {
	/**
	 * A handler callback for the {@link XMPPConnection#sendIQ(IQ)} method.
	 */
	interface IQHandler {
		/**
		 * Called if the IQ resulted in a successful <code>&lt;iq type="result" /&gt;</code> stanza
		 * @param iq the result stanza
		 */
		void onResult(IQ iq);

		/**
		 * Called if the IQ resulted in an error <code>&lt;iq type="error" /&gt;</code> stanza
		 * @param iq the error stanza
		 */
		void onError(IQ iq);
	}
	
	void send(Packet p);
	
	void sendIQ(IQ iq, IQHandler handler);
}
