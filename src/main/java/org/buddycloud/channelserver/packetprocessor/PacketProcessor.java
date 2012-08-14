package org.buddycloud.channelserver.packetprocessor;

import org.xmpp.packet.Packet;

public interface PacketProcessor<T extends Packet> {
    
    public void process(T packet) throws Exception;
    
}
