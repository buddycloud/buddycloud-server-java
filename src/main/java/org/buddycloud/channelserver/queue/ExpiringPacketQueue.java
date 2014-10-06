package org.buddycloud.channelserver.queue;

public class ExpiringPacketQueue extends ExpiringQueueAbstract {

    private static final long serialVersionUID = 1L;

    public ExpiringPacketQueue() {
        super();
    }

    public ExpiringPacketQueue(int timeout) {
        super(timeout);
    }

    @Override
    public void expireValue(Object value) {
        if (this.containsKey(value)) {
            this.remove(value);
        }
    }
}
