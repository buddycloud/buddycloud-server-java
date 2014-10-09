package org.buddycloud.channelserver.queue;

public class UnknownFederatedPacketException extends Exception {

    public UnknownFederatedPacketException(String message) {
        super(message);
    }

    private static final long serialVersionUID = 1L;
}
