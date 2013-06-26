package org.buddycloud.channelserver.queue;

public class ExpiringPacketQueue extends ExpiringQueueAbstract {

	public ExpiringPacketQueue() {
		super();
	}
	
	public ExpiringPacketQueue(int timeout) {
		super(timeout);
	}

	@Override
	public void expireValue(Object value) {
		if (this.containsKey(value)) this.remove(value);
	}
}