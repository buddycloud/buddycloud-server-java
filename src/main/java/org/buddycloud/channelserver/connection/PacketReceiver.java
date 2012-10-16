package org.buddycloud.channelserver.connection;

import org.xmpp.packet.Packet;

public interface PacketReceiver {
	/**
	 * Called when a packet is received.
	 * @param p the packet which has been received.
	 * @return <code>true</code> if the packet was handled, <code>false</code> otherwise.
	 */
	boolean packetReceived(Packet p);
}
