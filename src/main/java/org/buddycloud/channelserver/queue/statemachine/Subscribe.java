package org.buddycloud.channelserver.queue.statemachine;

import java.io.StringReader;
import java.util.Map;
import java.util.UUID;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.DataStoreException;

import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;

public class Subscribe extends AStatemachine  {

    public final static String EVENT_TYPE = "subscribe";
    
    public final static String STATE_SENT_SUBSCRIBE   = "onSubscribe";
    
    private Subscribe() {
        info.put(KEY_EVENT_TYPE, EVENT_TYPE); 
    }
    
    // Todo, refactor this.
    public Packet nextStep() throws DataStoreException {
        
        IQ nextIQ = this.discoverChannelServer();
        
        if(nextIQ != null) {
            dataStore.storeState(iq.getID(), nextIQ.getID(), info);
            return nextIQ;
        }
        
        /*
         * OK, if we are here, it means that we have already discovered who the 
         * foreign channel server. 
         * 
         * a.k.a honeymoon is over, let's rock!
         * 
         * First we check if we have a known state, and if not, the default 
         * state is the initial command what this type of state suppose to do.
         */
        
        if(info.get(KEY_STATE).equals(STATE_SENT_SUBSCRIBE)) {
            
            SAXReader xmlReader = new SAXReader();
            Element entry = null;
            
            try {
                entry = xmlReader.read(new StringReader(info.get(KEY_ORIGINAL_REQUEST))).getRootElement();
            } catch (DocumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            IQ oldIQ = new IQ(entry);
            
            String node = info.get(KEY_NODE);
            String jid = oldIQ.getFrom().toBareJID();
            String subscription = org.buddycloud.channelserver.pubsub.subscription.Subscriptions.unconfigured.toString();
            
            Element possiblePubsub = iq.getChildElement();
            if(possiblePubsub != null) {
                Element possibleSubscription = possiblePubsub.element("subscription");
                node = possibleSubscription.attributeValue("node");
                jid = possibleSubscription.attributeValue("jid");
                subscription = possibleSubscription.attributeValue("subscription");
            }
            
            nextIQ = IQ.createResultIQ(oldIQ);
            info.clear();
            
            nextIQ.setChildElement("pubsub", "http://jabber.org/protocol/pubsub")
                  .addElement("subscription")
                  .addAttribute("node", node)
                  .addAttribute("jid", jid)
                  .addAttribute("subscription", subscription);
            
            dataStore.subscribeUserToNode(jid, 
                                          node, 
                                          null, 
                                          subscription, 
                                          iq.getFrom().getDomain()); // If there is no node in the JID, 
                                                                     // this library set it's as null.
        
        } else {
                    
            // We have found a buddycloud channelserver
            // and we know it's JID is the iq.getFrom();
            
            SAXReader xmlReader = new SAXReader();
            Element entry = null;
            
            try {
                entry = xmlReader.read(new StringReader(info.get(KEY_ORIGINAL_REQUEST))).getRootElement();
            } catch (DocumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            nextIQ = new IQ(entry);
            
            nextIQ.setID(UUID.randomUUID().toString());
            nextIQ.getChildElement().addElement("actor", "http://buddycloud.org/v1").setText(nextIQ.getFrom().toBareJID());
            nextIQ.setFrom("willbe@overriden");
            nextIQ.setTo(iq.getFrom());
            
            info.put(KEY_STATE, STATE_SENT_SUBSCRIBE);
        }
        
        dataStore.storeState(iq.getID(), nextIQ.getID(), info);
        return nextIQ;
    }
    
    public static Subscribe buildSubscribeStatemachine(String node, IQ originalRequest, DataStore dataStore) {
        Subscribe s = new Subscribe();
        
        s.info.put(KEY_STATE, STATE_INIT);
        s.info.put(KEY_ORIGINAL_REQUEST, originalRequest.toXML());
        s.info.put(KEY_NODE, node);
        
        s.iq = originalRequest;
        
        s.dataStore = dataStore;
        
        return s;
    }
    
    public static Subscribe buildFromState(IQ iq, Map<String, String> state, DataStore dataStore) {
        Subscribe s = new Subscribe();
        
        s.info = state;
        s.iq = iq;
        s.dataStore = dataStore;
        
        return s;
    }
}
