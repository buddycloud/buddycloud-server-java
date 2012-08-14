package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class NodeCreate implements PubSubElementProcessor
{
    private static final Logger LOGGER = Logger.getLogger(PublishSet.class);
    
    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    public NodeCreate(BlockingQueue<Packet> outQueue, DataStore dataStore)
    {
        this.outQueue  = outQueue;
        this.dataStore = dataStore;
    }

    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception
    {
    	
    }

	@Override
	public boolean accept(Element elm) {
		// TODO Auto-generated method stub
		return false;
	}
}
