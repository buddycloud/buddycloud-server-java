package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliation;
import org.buddycloud.channelserver.pubsub.event.Event;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeConfigure extends PubSubElementProcessorAbstract
{	
	protected String   node;
	protected Document documentHelper;
	
	public NodeConfigure(BlockingQueue<Packet> outQueue, DataStore dataStore)
    {
    	setDataStore(dataStore);
    	setOutQueue(outQueue);
    }

	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) 
	    throws Exception
    {
    	element     = elm;
    	response    = IQ.createResultIQ(reqIQ);
    	request     = reqIQ;
    	actor       = actorJID;
        node        = element.attributeValue("node");
        try {
	        if ((false == nodeProvided())
	            || (false == nodeExists())
	            || (false == userCanModify())
	        ) {
	        	outQueue.put(response);
	        	return;
	        }
        } catch (DataStoreException e) {
        	setErrorCondition(
        		PacketError.Type.cancel,
        		PacketError.Condition.internal_server_error
        	);
        	outQueue.put(response);
        	return;
        }
        setNodeConfiguration();
    }

	private void setNodeConfiguration() throws Exception
	{
		try {
			getNodeConfigurationHelper().parse(request);
			if (true == getNodeConfigurationHelper().isValid()) {
				HashMap<String, String> configuration = getNodeConfigurationHelper().getValues();
				updateNodeConfiguration(configuration);
	            notifySubscribers();	
	            return;
			}
		} catch (NodeConfigurationException e) {
			setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
			outQueue.put(response);
			return;
		} catch (DataStoreException e) {
			setErrorCondition(PacketError.Type.cancel, PacketError.Condition.internal_server_error);
			outQueue.put(response);
			return;
		}
		setErrorCondition(PacketError.Type.modify, PacketError.Condition.bad_request);
		outQueue.put(response);
	}

	private void updateNodeConfiguration(HashMap<String, String> configuration) 
		throws Exception
	{
		dataStore.addNodeConf(node, configuration);
		outQueue.put(response);
	}

	private void notifySubscribers() throws DataStoreException, InterruptedException
	{
	    Iterator<? extends NodeSubscription> subscribers = dataStore.getNodeSubscribers(node);
	    Document document     = getDocumentHelper();
        Element message       = document.addElement("message");
        Element event         = message.addElement("event");
        Element configuration = event.addElement("configuration");
        configuration.addAttribute("node", node);
        event.addAttribute("xmlns", Event.NAMESPACE);
        message.addAttribute("id", request.getID());
        message.addAttribute("from", request.getTo().toString());
        Message rootElement = new Message(message);
        
		while (true == subscribers.hasNext()) {
			String subscriber = subscribers.next().getBareJID();
			message.addAttribute("to", subscriber);
            Message notification = rootElement.createCopy();
			outQueue.put(notification);
		}
	}

	private boolean userCanModify() throws DataStoreException
	{
		HashMap<String, String> nodeConfiguration = dataStore.getNodeConf(node);
		String owner = nodeConfiguration.get(Affiliation.OWNER.toString());
		if (true == owner.equals(actor.toString())) {
			return true;
		}
		setErrorCondition(PacketError.Type.auth, PacketError.Condition.forbidden);
		return false;
	}

	private boolean nodeExists() throws DataStoreException
	{
		if (true == dataStore.nodeExists(node)) {
			return true;
		}
		setErrorCondition(
			PacketError.Type.cancel,
			PacketError.Condition.item_not_found
		);
		return false;
	}

	private boolean nodeProvided()
	{
		if ((null != node) && !node.equals("")) {
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

	public boolean accept(Element elm)
	{
		return elm.getName().equals("configure");
	}
	
	public void setDocumentHelper(Document helper)
	{
		documentHelper = helper;
	}
	
	protected Document getDocumentHelper()
	{
		if (null == documentHelper) {
			documentHelper = DocumentHelper.createDocument();
		}
		return documentHelper;
	}
}