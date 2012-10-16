package org.buddycloud.channelserver.connection.iq;

import org.buddycloud.channelserver.connection.XMPPConnection;
import org.xmpp.packet.IQ;

public class IQRequestProcessor {
	private String idPrefix;
	private int requestCounter;
	
	private final XMPPConnection connection;
	
	public IQRequestProcessor(final XMPPConnection connection) {
		this.connection = connection;
		idPrefix = String.valueOf(super.hashCode());
		requestCounter = 0;
	}
	
	/**
	 * Processes an {@link IQRequest}, sending the request packet and calling the appropriate methods
	 * when a reply is received.
	 * <p>This method will automatically add a unique id to the {@link IQ} if it doesn't already have one.
	 * @param request the request to process
	 */
	public void processRequest(final IQRequest request) {
		IQ iq = request.getRequest();
		
		if(iq.getID() == null) {
			iq.setID(nextId());
		}
		
		connection.sendIQ(request.getRequest(), new XMPPConnection.IQHandler() {
			
			@Override
			public void onResult(IQ iq) {
				request.onResult(iq);
			}
			
			@Override
			public void onError(IQ iq) {
				request.onError(iq);
			}
		});
	}
	
	private synchronized String nextId() {
		return idPrefix + requestCounter++;
	}
}
