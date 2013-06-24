package org.buddycloud.channelserver.channel;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.ChannelsEngine;
import org.xmpp.packet.Packet;

public class ChannelsEngineMock extends ChannelsEngine {

	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();
	
	public ChannelsEngineMock() {
		super(new Properties());
	}

	public void sendPacket(Packet packet) {
		try {
		    queue.put(packet);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
	
	public void clear() {
	    queue.clear();	
	}
	
	public Packet poll() {
		return queue.poll();
	}
	
	public int size() {
		return queue.size();
	}
}
