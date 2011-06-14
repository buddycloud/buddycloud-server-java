package org.buddycloud.channels.queue;

import org.xmpp.packet.Packet;

public interface IOutQueue {

	public void put(Packet p);
	
}
