package org.buddycloud.channelserver.packetHander.IQ.Namespace;

import org.xmpp.packet.IQ;

public interface INamespace {
	
	public void ingestIQ(IQ iq);

}
