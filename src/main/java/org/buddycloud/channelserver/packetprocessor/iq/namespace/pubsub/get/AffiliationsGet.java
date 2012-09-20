package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Iterator;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.apache.log4j.Logger;

public class AffiliationsGet implements PubSubElementProcessor {

    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    private static final Logger LOGGER = Logger.getLogger(AffiliationsGet.class);
    
    public AffiliationsGet(BlockingQueue<Packet> outQueue, DataStore dataStore)
    {
        this.outQueue = outQueue;
        this.dataStore = dataStore;
    }
    
    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception
   {
        IQ result            = IQ.createResultIQ(reqIQ);
        Element pubsub       = result.setChildElement(PubSubGet.ELEMENT_NAME, JabberPubsub.NS_PUBSUB_OWNER);
        Element affiliations = pubsub.addElement("affiliations");
        
        String node = elm.attributeValue("node");

        if (actorJID == null) {
            actorJID = reqIQ.getFrom();
        }
        
        if (node == null) {
            // let's get all subscriptions.
            Iterator<? extends NodeSubscription> cur = dataStore
            	.getUserSubscriptionsOfNodes(actorJID.toBareJID());

            while (cur.hasNext()) {
                NodeSubscription ns = cur.next();
                LOGGER.trace("Adding affiliation for " + ns.getBareJID() + " affiliation " + ns.getAffiliation());
                affiliations.addElement("affiliation")
                            .addAttribute("node", ns.getNode())
                            .addAttribute("affiliation", ns.getAffiliation())
                            .addAttribute("jid", ns.getBareJID());
            }
        } else {
            // Let's get only one subscription.
        	Iterator<? extends NodeSubscription> cur = dataStore.getNodeSubscribers(node);
            affiliations.addAttribute("node", node);
            Element affiliation;
            while (cur.hasNext()) {
                NodeSubscription ns = cur.next();
            	LOGGER.trace("Adding affiliation for " + ns.getBareJID() + " affiliation " + ns.getAffiliation());
                
                affiliation = affiliations.addElement("affiliation");
                affiliation.addAttribute("node", ns.getNode())
                    .addAttribute("affiliation", ns.getAffiliation())
                    .addAttribute("jid", ns.getBareJID());
            }
        }
        outQueue.put(result);
    }

    @Override
    public boolean accept(Element elm)
    {
        return elm.getName().equals("affiliations");
    }
}
