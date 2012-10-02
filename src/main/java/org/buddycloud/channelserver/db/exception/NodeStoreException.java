package org.buddycloud.channelserver.db.exception;

public class NodeStoreException extends Exception {
	private static final long serialVersionUID = 1L;

	public NodeStoreException(final String message) {
		super(message);
	}

	public NodeStoreException(final String message, final Throwable t) {
		super(message, t);
	}

	public NodeStoreException(final Throwable t) {
		super(t);
	}

	public NodeStoreException() {
		super();
	}
}
