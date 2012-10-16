package org.buddycloud.channelserver.connection.iq;

import org.xmpp.packet.IQ;

public interface IQRequest {
	IQ getRequest();
	
	void onResult(IQ response);
	
	void onError(IQ error);
}
