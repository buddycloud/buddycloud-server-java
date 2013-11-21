package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.Helper;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public abstract class PubSubElementProcessorAbstract
    implements PubSubElementProcessor
{
	public static final String NS_RSM = "http://jabber.org/protocol/rsm";
    
	public static Logger logger = Logger.getLogger(PubSubElementProcessorAbstract.class);
	
    protected BlockingQueue<Packet> outQueue;
    protected ChannelManager        channelManager;
    protected Element               element;
    protected IQ                    response;
    protected IQ                    request;
    protected JID                   actor;
    protected String                serverDomain;
    protected String                topicsDomain;
    protected String                node = null;
    protected Helper                configurationHelper;

	protected Element resultSetManagement;
	protected String firstItem;
	protected String lastItem;
	protected int totalEntriesCount;

	private Collection<JID> adminUsers;

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

	protected Collection<JID> getAdminUsers() {
		if (null == adminUsers) {
			adminUsers = Configuration.getInstance().getAdminUsers();
		}
		return adminUsers;
	}
	
	public void setTopicsDomain(String domain)
	{
	    topicsDomain = domain;		
	}
	
	public void setNode(String node) {
        this.node = node;
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
		if (null == response) response = IQ.createResultIQ(request);
		response.setType(IQ.Type.error);
		PacketError error = new PacketError(condition, type);
		response.setError(error);
	}
	
	protected void createExtendedErrorReply(Type type, Condition condition,
			String additionalElement) {
		if (null == response) response = IQ.createResultIQ(request);
		response.setType(IQ.Type.error);
		Element standardError = new DOMElement(condition.toString(),
				new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
		Element extraError = new DOMElement(additionalElement,
				new org.dom4j.Namespace("", Buddycloud.NS_BUDDYCLOUD_ERROR));
		Element error = new DOMElement("error");
		error.addAttribute("type", type.toString());
		error.add(standardError);
		error.add(extraError);
		response.setChildElement(error);
	}
	
	protected Document getDocumentHelper()
	{
		return DocumentHelper.createDocument();
	}
}