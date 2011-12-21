package org.buddycloud.channelserver.packetHander.Message;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;
import java.util.TimeZone;
import java.util.UUID;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetHander.APacketHandler;
import org.buddycloud.channelserver.packetHander.IQ.Namespace.JabberPubsub;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.buddycloud.channelserver.queue.AOutQueue;
import org.buddycloud.channelserver.queue.InQueue;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;

import com.mongodb.DBCursor;

public class MessageHandler extends APacketHandler {

    private Logger LOGGER = Logger.getLogger(MessageHandler.class);
    
    public InQueue inQueue;
    
	public MessageHandler(AOutQueue outQueue, InQueue inQueue, Properties conf, DataStore dataStore) {
		this.outQueue  = outQueue;
		this.dataStore = dataStore;
		this.inQueue   = inQueue;
	}
	
	public void ingestMessage(Message msg) {
		
        LOGGER.debug("Message handler got body '" + msg.getBody() + "'.");
        
        if(dataStore.isLocalUser(msg.getFrom().toBareJID())) {
            
            String[] privateBodyparts = msg.getBody().split(" ", 2);
            if (privateBodyparts[0].equals(":subscribe")) {
                
                LOGGER.debug("Got subscribe command. Rest of the body '" + privateBodyparts[1] + "'.");
                
                IQ iq = new IQ();
                iq.setType(IQ.Type.set);
                iq.setTo("channelserver");
                iq.setFrom(msg.getFrom());
                
                Element pubsub = iq.setChildElement("pubsub", "http://jabber.org/protocol/pubsub");
                pubsub.addElement("subscribe")
                      .addAttribute("node", "/user/" + privateBodyparts[1] + "/posts")
                      .addAttribute("jid", msg.getFrom().toBareJID());
                
                this.inQueue.put(iq.toXML());
                
            } else if (privateBodyparts[0].equals(":unsubscribe")) {
                
                LOGGER.debug("Got unsubscribe command. Rest of the body '" + privateBodyparts[1] + "'.");
                
                IQ iq = new IQ();
                iq.setType(IQ.Type.set);
                iq.setTo("channelserver");
                iq.setFrom(msg.getFrom());
                
                Element pubsub = iq.setChildElement("pubsub", "http://jabber.org/protocol/pubsub");
                pubsub.addElement("unsubscribe")
                      .addAttribute("node", "/user/" + privateBodyparts[1] + "/posts")
                      .addAttribute("jid", msg.getFrom().toBareJID());
                
                this.inQueue.put(iq.toXML());
                
            } else if(privateBodyparts[0].equals(":publish")) {
                
                String receiver = msg.getFrom().toBareJID();
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
                mockIQ.setTo(msg.getTo());
                mockIQ.setFrom(msg.getFrom());
                
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
                author.addElement("name").setText(msg.getFrom().toBareJID());
                author.addElement("jid", "http://buddycloud.com/atom-elements-0").setText(msg.getFrom().toBareJID());
                
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
                this.inQueue.put(mockIQ.toXML());
                
            } else if (privateBodyparts[0].equals(":subscriptions")) {
                
                LOGGER.debug("Got subscriptions command.");
                
                IQ iq = new IQ();
                iq.setType(IQ.Type.get);
                iq.setTo("channelserver");
                iq.setFrom(msg.getFrom());
                
                Element pubsub = iq.setChildElement("pubsub", "http://jabber.org/protocol/pubsub");
                pubsub.addElement("subscriptions");
                
                this.inQueue.put(iq.toXML());
            
            } else {
                LOGGER.debug("Sorry, did not understand this command '" + privateBodyparts[0] + "' (yet).");
            }
        }
	
        Element event = msg.getChildElement("event", "http://jabber.org/protocol/pubsub#event");
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
        
        msg.setBody(":publish " + sentToNode + ":" + itemID + " " + whoSays + " wrote: " + body);
        msg.setFrom(msg.getTo());
        
        DBCursor cur = dataStore.getNodeSubscribers(node);
        while(cur.hasNext()) {
            NodeSubscription ns = (NodeSubscription) cur.next();
            String toBareJID = ns.getBareJID();
            
            msg.setTo(toBareJID);
            outQueue.put(msg.createCopy());
        }
	}
}
