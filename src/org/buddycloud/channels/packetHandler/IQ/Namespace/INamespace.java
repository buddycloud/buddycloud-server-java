package org.buddycloud.channels.packetHandler.IQ.Namespace;

import org.xmpp.packet.Packet;

public interface INamespace {
	
	public void ingestPacket(Packet p);

}
