package org.buddycloud.channelserver.channel;

public class BuddycloudException extends Exception {

    private static final long serialVersionUID = 1L;

    public BuddycloudException() {
        super();
    }

    public BuddycloudException(String message) {
        super(message);
    }

    public BuddycloudException(Throwable cause) {
        super(cause);
    }

    public BuddycloudException(String message, Throwable cause) {
        super(message, cause);
    }
}
