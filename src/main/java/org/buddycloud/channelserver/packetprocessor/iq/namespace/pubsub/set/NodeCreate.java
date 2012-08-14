package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.IQ.Type;

public class NodeCreate implements PubSubElementProcessor
{
    private static final Logger LOGGER = Logger.getLogger(NodeCreate.class);
    
    private final BlockingQueue<Packet> outQueue;
    private final DataStore             dataStore;
    private Element                     element;
    private IQ                          response;
    
    
    public NodeCreate(BlockingQueue<Packet> outQueue, DataStore dataStore)
    {
        this.outQueue  = outQueue;
        this.dataStore = dataStore;
    }

    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception
    {
    	element  = elm;
    	response = IQ.createResultIQ(reqIQ);

    	if (false == validateNode()) {
    		return;
    	}
        
    }

	@Override
	public boolean accept(Element elm)
	{
		return elm.getName().equals("create");
	}
	
	private boolean validateNode() throws InterruptedException
	{
        String node = element.attributeValue("node");
        if (node == null || node.equals("")) {
        	response.setType(Type.error);
        	Element nodeIdRequired = new DOMElement(
                "nodeid-required",
                new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR)
            );
        	Element badRequest = new DOMElement(
        	    PacketError.Condition.bad_request.toString(),
                new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS)
        	);
            Element error = new DOMElement("error");
            error.addAttribute("type", "modify");
            error.add(badRequest);
            error.add(nodeIdRequired);
            response.setChildElement(error);
            outQueue.put(response);
            return false;
        }
        return true;
	}
}
