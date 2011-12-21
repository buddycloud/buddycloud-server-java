package org.buddycloud.channelserver.packetHander;

import org.xmpp.packet.Packet;

public interface IPacketHandler {
	public void ingestPacket(Packet p);
}
