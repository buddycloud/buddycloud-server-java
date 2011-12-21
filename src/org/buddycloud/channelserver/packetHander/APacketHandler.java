package org.buddycloud.channelserver.packetHander;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.queue.AOutQueue;


public abstract class APacketHandler {

	public AOutQueue outQueue;
	public DataStore dataStore;
//	public ErrorQueue errorQueue;
//	protected Jedis jedis;
//	
}
