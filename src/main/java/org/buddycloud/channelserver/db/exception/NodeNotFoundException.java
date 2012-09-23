package org.buddycloud.channelserver.db.exception;

public class NodeNotFoundException extends NodeStoreException {
	private static final long serialVersionUID = 1L;

	public NodeNotFoundException(String message) {
		super(message);
	}

	public NodeNotFoundException(String message, Throwable t) {
		super(message, t);
	}

	public NodeNotFoundException(Throwable t) {
		super(t);
	}

}
