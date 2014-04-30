package org.buddycloud.channelserver.channel.validate;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.dom4j.Element;
import org.xmpp.packet.JID;

public interface PayloadValidator {

	public abstract void setPayload(Element payload);

	public abstract String getErrorMessage();

	public abstract void setChannelManager(ChannelManager channelManager);

	/**
	 * @throws InterruptedException
	 * @throws NodeStoreException
	 */
	public abstract boolean isValid() throws NodeStoreException;

	public abstract Element getPayload();

	public abstract void setUser(JID jid);

	public abstract void setNode(String node);

	public abstract void setTo(String channelServerDomain);

    public abstract String getLocalItemId();

    public abstract String getGlobalItemId();

    public abstract String getInReplyTo();

    public abstract boolean canValidate(String contentType);
}