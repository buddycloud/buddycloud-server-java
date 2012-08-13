package org.buddycloud.channelserver.packetprocessor.message;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Properties;
import java.util.TimeZone;
import java.util.UUID;
import java.util.concurrent.BlockingQueue;


import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;

public class MessageProcessor implements PacketProcessor<Message> {

    private static final Logger LOGGER = Logger.getLogger(MessageProcessor.class);
    
    private final BlockingQueue<Packet> inQueue;
    private final BlockingQueue<Packet> outQueue;
    private DataStore dataStore;
    
	public MessageProcessor(BlockingQueue<Packet> outQueue, 
	        BlockingQueue<Packet> inQueue, Properties conf, DataStore dataStore) {
		this.outQueue  = outQueue;
		this.dataStore = dataStore;
		this.inQueue   = inQueue;
	}
	
    @Override
    public void process(Message packet) throws Exception {
        
        LOGGER.debug("Message handler got body '" + packet.getBody() + "'.");
        
        if(dataStore.isLocalUser(packet.getFrom().toBareJID())) {
            
            String[] privateBodyparts = packet.getBody().split(" ", 2);
            if (privateBodyparts[0].equals(":subscribe")) {
                
                LOGGER.debug("Got subscribe command. Rest of the body '" + privateBodyparts[1] + "'.");
                
                IQ iq = new IQ();
                iq.setType(IQ.Type.set);
                iq.setTo("channelserver");
                iq.setFrom(packet.getFrom());
                
                Element pubsub = iq.setChildElement("pubsub", "http://jabber.org/protocol/pubsub");
                pubsub.addElement("subscribe")
                      .addAttribute("node", "/user/" + privateBodyparts[1] + "/posts")
                      .addAttribute("jid", packet.getFrom().toBareJID());
                
                this.inQueue.put(iq);
                
            } else if (privateBodyparts[0].equals(":unsubscribe")) {
                
                LOGGER.debug("Got unsubscribe command. Rest of the body '" + privateBodyparts[1] + "'.");
                
                IQ iq = new IQ();
                iq.setType(IQ.Type.set);
                iq.setTo("channelserver");
                iq.setFrom(packet.getFrom());
                
                Element pubsub = iq.setChildElement("pubsub", "http://jabber.org/protocol/pubsub");
                pubsub.addElement("unsubscribe")
                      .addAttribute("node", "/user/" + privateBodyparts[1] + "/posts")
                      .addAttribute("jid", packet.getFrom().toBareJID());
                
                this.inQueue.put(iq);
                
            } else if(privateBodyparts[0].equals(":publish")) {
                
                String receiver = packet.getFrom().toBareJID();
                String replyTo = null;
                if(privateBodyparts[1].startsWith("@")) {
                    String[] privateBodypartsTwo = privateBodyparts[1].split(" ", 2);
                    receiver = privateBodypartsTwo[0].substring(1);
                    
                    String[] privateBodypartsThree = receiver.split(":", 2);
                    
                    if(privateBodypartsThree.length > 1) {
                        replyTo = privateBodypartsThree[1];
                        receiver = privateBodypartsThree[0];
                    }
                    
                    privateBodyparts[1] = privateBodypartsTwo[1];
                }
                
                IQ mockIQ = new IQ();
                mockIQ.setType(IQ.Type.set);
                mockIQ.setID(UUID.randomUUID().toString());
                mockIQ.setTo(packet.getTo());
                mockIQ.setFrom(packet.getFrom());
                
                Element pubsub = mockIQ.setChildElement("pubsub", JabberPubsub.NAMESPACE_URI);
                Element publish = pubsub.addElement("publish");
                publish.addAttribute("node", "/user/" + receiver + "/posts");
                Element item = publish.addElement("item"); 
                
                Element entry = new DOMElement("entry", new org.dom4j.Namespace("", "http://www.w3.org/2005/Atom"));
                entry.add(new org.dom4j.Namespace("activity", "http://activitystrea.ms/spec/1.0/"));
                
                String DATE_FORMAT = "yyyy-MM-dd'T'H:m:s'Z'";
                SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
                sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

                entry.addElement("id").setText("1");
                entry.addElement("title").setText("Post");
                
                entry.addElement("content")
                     //.addAttribute("type", "text")
                     .setText(privateBodyparts[1]);
                
                entry.addElement("published").setText(sdf.format(new Date()));
                entry.addElement("updated").setText(sdf.format(new Date()));
                
                Element author = entry.addElement("author");
                author.addElement("name").setText(packet.getFrom().toBareJID());
                author.addElement("jid", "http://buddycloud.com/atom-elements-0").setText(packet.getFrom().toBareJID());
                
//                Element geoloc = entry.addElement("geoloc", "http://jabber.org/protocol/geoloc");
//                geoloc.addElement("text").setText("Home Sweet Home!");
//                geoloc.addElement("locality").setText("Paris");
//                geoloc.addElement("country").setText("Ffance");
                
                entry.addElement("activity:verb")
                     .setText("post");
                entry.addElement("activity:object")
                     .addElement("activity:object-type")
                     .setText("note");
                
                if(replyTo != null) {
                    entry.addElement("in-reply-to", "http://purl.org/syndication/thread/1.0").addAttribute("ref", replyTo);
                }
                
                item.add(entry);
                this.inQueue.put(mockIQ);
                
            } else if (privateBodyparts[0].equals(":subscriptions")) {
                
                LOGGER.debug("Got subscriptions command.");
                
                IQ iq = new IQ();
                iq.setType(IQ.Type.get);
                iq.setTo("channelserver");
                iq.setFrom(packet.getFrom());
                
                Element pubsub = iq.setChildElement("pubsub", "http://jabber.org/protocol/pubsub");
                pubsub.addElement("subscriptions");
                
                this.inQueue.put(iq);
            
            } else {
                LOGGER.debug("Sorry, did not understand this command '" + privateBodyparts[0] + "' (yet).");
            }
        }
    
        Element event = packet.getChildElement("event", "http://jabber.org/protocol/pubsub#event");
        if(event == null) {
            return;
        }
        
        Element items = event.element("items");
        if(items == null) {
            return;
        }
        String node = items.attributeValue("node");
        if(node == null) {
            return;
        }
        
        // Let's modify the package a bit.
        String[] splittedNode = node.split("/");
        String sentToNode = "@" + splittedNode[2];
        String itemID     = items.element("item").attributeValue("id");
        String whoSays    = items.element("item").element("entry").element("author").element("name").getText();
        String body       = items.element("item").element("entry").element("content").getText();
        
        packet.setBody(":publish " + sentToNode + ":" + itemID + " " + whoSays + " wrote: " + body);
        packet.setFrom(packet.getTo());
        
        Iterator<? extends NodeSubscription> cur = dataStore.getNodeSubscribers(node);
        while(cur.hasNext()) {
            NodeSubscription ns = cur.next();
            String toBareJID = ns.getBareJID();
            
            packet.setTo(toBareJID);
            outQueue.put(packet.createCopy());
        }
    }
}
