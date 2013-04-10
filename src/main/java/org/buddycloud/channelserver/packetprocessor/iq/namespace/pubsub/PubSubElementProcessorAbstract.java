package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.concurrent.BlockingQueue;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public abstract class PubSubElementProcessorAbstract
    implements PubSubElementProcessor
{
    
    protected BlockingQueue<Packet> outQueue;
    protected ChannelManager        channelManager;
    protected Element               element;
    protected IQ                    response;
    protected IQ                    request;
    protected JID                   actor;
    protected String                serverDomain;
    protected String                topicsDomain;
    protected String                node;
    protected Helper                configurationHelper;

	public void setOutQueue(BlockingQueue<Packet> outQueue)
	{
		this.outQueue = outQueue;
	}

	public void setChannelManager(ChannelManager channelManager)
	{
		this.channelManager = channelManager;
	}
	
	public void setServerDomain(String domain)
	{
		serverDomain = domain;
	}

	protected String getServerDomain()
	{
		if (null == serverDomain) {
            serverDomain = Configuration.getInstance()
			    .getProperty("server.domain");
		}
		return serverDomain;
	}

	public void setTopicsDomain(String domain)
	{
	    topicsDomain = domain;		
	}
	
	protected String getTopicsDomain()
	{
		if (null == topicsDomain) {
            topicsDomain = Configuration.getInstance()
			    .getProperty("server.domain.topics");
		}
		return topicsDomain;		
	}

	public void setConfigurationHelper(Helper helper)
	{
		configurationHelper = helper;
	}
	
	protected Helper getNodeConfigurationHelper()
	{
		if (null == configurationHelper) {
			configurationHelper = new Helper();
		}
		return configurationHelper;
	}
	
	protected void setErrorCondition(Type type, Condition condition)
	{
		response.setType(IQ.Type.error);
		PacketError error = new PacketError(condition, type);
		response.setError(error);
	}
	
	protected Document getDocumentHelper()
	{
		return DocumentHelper.createDocument();
	}
}