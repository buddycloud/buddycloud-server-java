package org.buddycloud.channelserver.db.exception;

public class ItemNotFoundException extends NodeStoreException {
    private static final long serialVersionUID = 1L;

    public ItemNotFoundException(String message, Throwable t) {
        super(message, t);
    }

    public ItemNotFoundException(String message) {
        super(message);
    }

    public ItemNotFoundException(Throwable t) {
        super(t);
    }

}
