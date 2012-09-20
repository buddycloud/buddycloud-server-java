package org.buddycloud.channelserver.db.exception;

public class NodeNotFoundException extends NodeStoreException {

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
