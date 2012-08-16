package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class NodeCreate implements PubSubElementProcessor
{
    private static final Logger LOGGER = Logger.getLogger(NodeCreate.class);
    
    private BlockingQueue<Packet> outQueue;
    private DataStore             dataStore;
	private Element               element;
    private IQ                    response;
    private JID                   actor;
	private String                serverDomain;
	private String                topicsDomain;
	private String                node;
	
	private static final Pattern nodeExtract = Pattern.compile("^/user/[^@]+@([^/]+)/[^/]+$");
    private static final String NODE_REG_EX  = "^/user/[^@]+@[^/]+/[^/]+$";
	
	public NodeCreate(BlockingQueue<Packet> outQueue, DataStore dataStore)
    {
    	setDataStore(dataStore);
    	setOutQueue(outQueue);
    }

	public void setOutQueue(BlockingQueue<Packet> outQueue)
	{
		this.outQueue = outQueue;
	}

	public void setDataStore(DataStore dataStore)
	{
		this.dataStore = dataStore;
	}

	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) 
	    throws Exception
    {
    	element     = elm;
    	response    = IQ.createResultIQ(reqIQ);
    	actor       = actorJID;
        node        = element.attributeValue("node");
    	if ((false == validateNode()) 
    	    || (true == doesNodeExist())
    	    || (false == actorIsRegistered())
    	    || (false == nodeHandledByThisServer())
    	) {
            outQueue.put(response);
    		return;
    	}
    	createNode();
        
    }

	private void createNode() throws InterruptedException
	{
		try {
		    dataStore.createNode(actor.toString(), node, getNodeConfiguration());
		} catch (DataStoreException e) {
			setErrorCondition(
			    PacketError.Type.wait,
			    PacketError.Condition.internal_server_error
			);
			outQueue.put(response);
			return;
		}
		response.setType(IQ.Type.result);
		outQueue.put(response);
	}

	private Map<String, String> getNodeConfiguration()
	{
		HashMap<String, String> configuration = new HashMap<String, String>();
		return configuration;
	}

	public boolean accept(Element elm)
	{
		return elm.getName().equals("create");
	}
	
	private boolean validateNode()
	{
        if (node != null && !node.trim().equals("")) {
        	return true;
        }
    	response.setType(IQ.Type.error);
    	Element nodeIdRequired = new DOMElement(
            "nodeid-required",
            new Namespace("", JabberPubsub.NS_PUBSUB_ERROR)
        );
    	Element badRequest = new DOMElement(
    	    PacketError.Condition.bad_request.toString(),
            new Namespace("", JabberPubsub.NS_XMPP_STANZAS)
    	);
        Element error = new DOMElement("error");
        error.addAttribute("type", "modify");
        error.add(badRequest);
        error.add(nodeIdRequired);
        response.setChildElement(error);
        return false;
	}
	
	private boolean doesNodeExist()
	{
		if (false == dataStore.nodeExists(node)) {
			return false;
		}
		setErrorCondition(
			PacketError.Type.cancel,
		    PacketError.Condition.conflict
		);
		return true;
	}
	
	private boolean actorIsRegistered()
	{
		if (true == actor.getDomain().equals(getServerDomain())) {
			return true;
		}
		setErrorCondition(
			PacketError.Type.auth,
		    PacketError.Condition.forbidden
		);
		return false;
	}

	private boolean nodeHandledByThisServer()
	{
		if (false == node.matches(NODE_REG_EX)) {
			setErrorCondition(
				PacketError.Type.modify,
			    PacketError.Condition.bad_request
			);
			return false;
		}
		Matcher matcher = nodeExtract.matcher(node);
		matcher.find();
		String  nodeDomain = matcher.group(1);

		if ((false == getServerDomain().equals(nodeDomain)) 
		    && (false == getTopicsDomain().equals(nodeDomain))
		) {
			setErrorCondition(
			    PacketError.Type.modify,
			    PacketError.Condition.not_acceptable
			);
			return false;
		}
		return true;
	}

	public void setServerDomain(String domain)
	{
		serverDomain = domain;
	}

	private String getServerDomain()
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
	
	private String getTopicsDomain()
	{
		if (null == topicsDomain) {
            serverDomain = Configuration.getInstance()
			    .getProperty("server.domain.topics");
		}
		return topicsDomain;		
	}
	
	private void setErrorCondition(Type type, Condition condition)
	{
		response.setType(IQ.Type.error);
		PacketError error = new PacketError(condition, type);
		response.setError(error);
	}
}