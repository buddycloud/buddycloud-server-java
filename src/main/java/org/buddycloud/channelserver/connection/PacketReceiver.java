package org.buddycloud.channelserver.connection;

import org.xmpp.packet.Packet;

public interface PacketReceiver {
	/**
	 * Called when a packet is received.
	 * @param p the packet which has been received.
	 */
	void packetReceived(Packet p);
}
