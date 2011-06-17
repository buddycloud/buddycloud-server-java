package org.buddycloud.channels.packetHandler;

import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.queue.OutQueue;

import redis.clients.jedis.Jedis;

public abstract class APacketHandler {

	public OutQueue outQueue;
	public ErrorQueue errorQueue;
	protected Jedis jedis;
	
}
