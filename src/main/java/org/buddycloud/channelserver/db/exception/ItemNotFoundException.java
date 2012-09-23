package org.buddycloud.channelserver.db.exception;

public class ItemNotFoundException extends NodeStoreException {
	private static final long serialVersionUID = 1L;

	public ItemNotFoundException(String message, Throwable t) {
		super(message, t);
		// TODO Auto-generated constructor stub
	}

	public ItemNotFoundException(String message) {
		super(message);
		// TODO Auto-generated constructor stub
	}

	public ItemNotFoundException(Throwable t) {
		super(t);
		// TODO Auto-generated constructor stub
	}

}
