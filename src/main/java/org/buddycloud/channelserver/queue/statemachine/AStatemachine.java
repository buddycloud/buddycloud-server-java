package org.buddycloud.channelserver.queue.statemachine;

import java.io.StringReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;


import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.PacketError;

public abstract class AStatemachine implements IStatemachine {

    protected static Logger LOGGER = Logger.getLogger("AStatemachine");
    
    protected Map<String, String> info = new HashMap<String, String>(); 
    
    public final static String STATE_SENT_DISCO_ITEMS = "onDiscoItems";
    public final static String STATE_SENT_DISCO_INFO  = "onDiscoInfo";

    public final static String KEY_ITEMS = "items";
    
    protected IQ iq;
    
    protected DataStore dataStore;
    
    // Refactor this one day.
    protected IQ discoverChannelServer() {
        
        IQ nextIQ = null;
        
        if(info.get(KEY_STATE).equals(STATE_INIT)) {
            
            String[] splittedNode = this.info.get(KEY_NODE).split("/");
                
            // 0 1    2   3
            //  /user/JID/something
            
            JID nodeJID = new JID(splittedNode[2]);
            
            /*
              We'll be sending this:
              http://xmpp.org/extensions/xep-0030.html
              4. Discovering the Items Associated with a Jabber Entity
              
                <iq type='get'
                    from='romeo@montague.net/orchard'
                    to='shakespeare.lit'
                    id='items1'>
                  <query xmlns='http://jabber.org/protocol/disco#items'/>
                </iq>
                 
             */
            
            nextIQ = new IQ();
            nextIQ.setType(Type.get);
            nextIQ.setFrom("willbe@overriden");
            nextIQ.setTo(nodeJID.getDomain());
            nextIQ.setID(UUID.randomUUID().toString());
            
            nextIQ.setChildElement("query", "http://jabber.org/protocol/disco#items");
            
            this.info.put(KEY_STATE, STATE_SENT_DISCO_ITEMS);
            
        } else if(info.get(KEY_STATE).equals(STATE_SENT_DISCO_ITEMS)) {
            
            nextIQ = this.checkErrorDuringDiscoProcess();
            if(nextIQ != null) {
                return nextIQ;
            }
                
//                SAXReader xmlReader = new SAXReader();
//                Element entry = null;
//                
//                try {
//                    entry = xmlReader.read(new StringReader(info.get(KEY_ORIGINAL_REQUEST))).getRootElement();
//                } catch (DocumentException e) {
//                    // TODO Auto-generated catch block
//                    e.printStackTrace();
//                }
//                IQ oldIQ = new IQ(entry);
//                
//                nextIQ = IQ.createResultIQ(oldIQ);
//                info.clear();
//                
//                nextIQ.setType(Type.error);
//                PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.item_not_found, 
//                                                 org.xmpp.packet.PacketError.Type.cancel);
//                nextIQ.setError(pe);
                
            //} else {
            
            /*
               We should receive something like this
               
                <iq type="result"
                    id="items1"
                    to="channels.denmark.lit"
                    from="shakespeare.lit">
                   <query xmlns='http://jabber.org/protocol/disco#items'>
                      <item jid='people.shakespeare.lit' name='Directory of Characters'/>
                      <item jid='channels.shakespeare.lit' name='Le awesome buddycloud channel server'/>
                   </query>
                </iq>
             */
            
            Element query = iq.getChildElement();
            @SuppressWarnings("unchecked")
            List<Element> items = query.elements("item"); 
            
            if(items.isEmpty()) {
                // If no items, time to stop.
                info.clear();
                // TODO!! Return error here!    
            }
            
            String firstComponent = null;
            StringBuilder commaSeparatedListOfComponents = new StringBuilder();
            for (Element item : items) {
                if(firstComponent == null) {
                    firstComponent = item.attributeValue("jid");
                    continue;
                }
                commaSeparatedListOfComponents.append(item.attributeValue("jid"));
                commaSeparatedListOfComponents.append(",");
            }
            
            // Store all the items as comma separated
            if(commaSeparatedListOfComponents.length() > 0) {
                info.put(KEY_ITEMS, commaSeparatedListOfComponents.substring(0, commaSeparatedListOfComponents.length() - 1));
            } else {
                info.put(KEY_ITEMS, "");
            }
            
            /*
              Let's send something like this
              
                <iq type="get"
                    id="items1"
                    from="willbe@overriden"
                    to="people.shakespeare.lit">
                   <query xmlns="http://jabber.org/protocol/disco#info"/>
                </iq>
             */
            
            nextIQ = new IQ();
            nextIQ.setType(Type.get);
            nextIQ.setFrom("willbe@overriden");
            nextIQ.setTo(firstComponent);
            nextIQ.setID(UUID.randomUUID().toString());
            
            nextIQ.setChildElement("query", "http://jabber.org/protocol/disco#info");
            
            info.put(KEY_STATE, STATE_SENT_DISCO_INFO);
            //}
            
        } else if(info.get(KEY_STATE).equals(STATE_SENT_DISCO_INFO)) {
            
            nextIQ = this.checkErrorDuringDiscoProcess();
            if(nextIQ != null) {
                return nextIQ;
            }
            
            /*
               We should receive something like this
            
                <iq type="result"
                    from="people.shakespeare.lit"
                    to="channels.denmark.lit"
                    id="items1">
                   <query xmlns="http://jabber.org/protocol/disco#info">
                      <identity category="pubsub" type="directory"/>
                   </query>
                </iq>
             */
            
            Element query = iq.getChildElement();
            //Element identity = query.element("identity");
            
            List<Element> identities = query.elements("identity");
            for (Element identity : identities) {
                if(identity.attributeValue("category") != null &&
                   identity.attributeValue("category").equals("pubsub") && 
                   identity.attributeValue("type") != null && 
                   identity.attributeValue("type").equals("channels")) {
                    
                    // We have found a buddycloud channel!
                    return null; // this means the caller of this methog knows
                                 // there is nothing to do for discoInfo anymore and
                                 // the iq.getFrom() is the right JID for the foreign
                                 // channel server.
                }
            }
              
            // Was not a buddycloud channel. Let's look for the next one.
            String[] items = info.get(KEY_ITEMS).split(",");
            
            if(items.length == 0) {
                // TODO! 
                // did not find a channel component.
                System.out.println("TODO! FIX THIS! Did not find channel component and chanenls run out!");
            }
            
            String firstComponent = null;
            StringBuilder commaSeparatedListOfComponents = new StringBuilder();
            
            for (int i = 0; i < items.length; i++) {
                if(firstComponent == null) {
                    firstComponent = items[i];
                    continue;
                }
                commaSeparatedListOfComponents.append(items[i]);
                commaSeparatedListOfComponents.append(",");
            }
            
            // Store all the items as comma separated
            if (commaSeparatedListOfComponents.length() > 0) {
                info.put(KEY_ITEMS, commaSeparatedListOfComponents.substring(0, commaSeparatedListOfComponents.length() - 1));
            }
            
            nextIQ = new IQ();
            nextIQ.setType(Type.get);
            nextIQ.setFrom("willbe@overriden");
            nextIQ.setTo(firstComponent);
            nextIQ.setID(UUID.randomUUID().toString());
            
            nextIQ.setChildElement("query", "http://jabber.org/protocol/disco#info");
            
            info.put(KEY_STATE, STATE_SENT_DISCO_INFO);            
        } else {
            
            // We might have an error on any state machine event (for example not allowed
            // to subscribe).
            //
            // So let's put the handling here, since currently they are all handled the same way.
            if(iq.getType() != IQ.Type.error) {
                LOGGER.debug("Given state '" + info.get(KEY_STATE) + "' is not a discoInfo state. Good! Dude, you are going somewhere!");
                return null;
            }
            
            nextIQ = new IQ();
            
            SAXReader xmlReader = new SAXReader();
            Element entry = null;
            
            try {
                entry = xmlReader.read(new StringReader(info.get(KEY_ORIGINAL_REQUEST))).getRootElement();
            } catch (DocumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            IQ oldIQ = new IQ(entry);
            
            nextIQ = IQ.createResultIQ(oldIQ);
            info.clear();
            
            nextIQ.setType(Type.error);
            nextIQ.setChildElement(iq.getError().getElement().createCopy());
            
        }
        
        return nextIQ;
        
    }
    
    private IQ checkErrorDuringDiscoProcess() {
        
        if(iq.getType() != IQ.Type.error) {
            return null;
        }
        
        IQ nextIQ = new IQ();
        
        SAXReader xmlReader = new SAXReader();
        Element entry = null;
        
        try {
            entry = xmlReader.read(new StringReader(info.get(KEY_ORIGINAL_REQUEST))).getRootElement();
        } catch (DocumentException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        IQ oldIQ = new IQ(entry);
        
        nextIQ = IQ.createResultIQ(oldIQ);
        info.clear();
        
        nextIQ.setType(Type.error);
        PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.item_not_found, 
                                         org.xmpp.packet.PacketError.Type.cancel);
        nextIQ.setError(pe);
        return nextIQ;
    }
}
