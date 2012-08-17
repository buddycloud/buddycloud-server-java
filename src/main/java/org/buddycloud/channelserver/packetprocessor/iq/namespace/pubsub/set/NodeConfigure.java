package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.concurrent.BlockingQueue;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class NodeConfigure extends PubSubElementProcessorAbstract
{
    private static final Logger LOGGER = Logger.getLogger(NodeConfigure.class);
	
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

    }

	public boolean accept(Element elm)
	{
		return elm.getName().equals("configure");
	}
}