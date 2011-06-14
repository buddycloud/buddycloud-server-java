package org.buddycloud.channels.packetHandler;

import org.xmpp.packet.Packet;

public interface IPacketHandler {
	public void ingestPacket(Packet p);
}
