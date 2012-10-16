package org.buddycloud.channelserver.connection;

import java.util.HashMap;
import java.util.Queue;

import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;


public class ComponentXMPPConnection implements XMPPConnection, PacketReceiver {

	private final Queue<Packet> outQueue;
	private HashMap<String, IQHandler> iqHandlers;
	
	public ComponentXMPPConnection(final Queue<Packet> outQueue) {
		this.outQueue = outQueue;
		this.iqHandlers = new HashMap<String, IQHandler>();
	}
	
	@Override
	public void send(Packet p) {
		outQueue.add(p);
	}

	@Override
	public void sendIQ(IQ iq, IQHandler handler) {
		this.iqHandlers.put(iq.getID(), handler);
		send(iq);
	}

	@Override
	public boolean packetReceived(Packet p) {
		if(p instanceof IQ) {
			IQ iq = (IQ) p;
			
			if((iq.getType() == IQ.Type.result) || (iq.getType() == IQ.Type.error)) {
				IQHandler handler = iqHandlers.get(iq.getID());
				
				if(handler != null) {
					if(iq.getType() == IQ.Type.result) {
						handler.onResult(iq);
					} else {
						handler.onError(iq);
					}

					return true;
				}
			}
		}
		return false;
	}

}
