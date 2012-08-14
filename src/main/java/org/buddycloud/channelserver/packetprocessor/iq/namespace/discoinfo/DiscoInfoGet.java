package org.buddycloud.channelserver.packetprocessor.iq.namespace.discoinfo;

import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.BlockingQueue;


import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.queue.statemachine.DiscoInfo;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class DiscoInfoGet implements PacketProcessor<IQ> {

    public static final String ELEMENT_NAME = "query";
    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    public DiscoInfoGet(BlockingQueue<Packet> outQueue, DataStore dataStore) {
        this.outQueue = outQueue;
        this.dataStore = dataStore;
    }

    @Override
    public void process(IQ reqIQ) throws Exception {
        
        IQ result = IQ.createResultIQ(reqIQ);
        Element elm = reqIQ.getChildElement();
        String node = elm.attributeValue("node");
        
        if(node == null) {
            Element query = result.setChildElement(ELEMENT_NAME, JabberDiscoInfo.NAMESPACE_URI);
            query.addElement("identity")
                 .addAttribute("category", "pubsub")
                 .addAttribute("type", "channels");
            
            query.addElement("identity")
                 .addAttribute("category", "pubsub")
                 .addAttribute("type", "inbox");
            
            query.addElement("feature")
                 .addAttribute("var", "jabber:iq:register");
            
            query.addElement("feature")
                 .addAttribute("var", "http://jabber.org/protocol/disco#info");
            
            outQueue.put(result);
            return;
        }
        
        Map<String, String> conf = dataStore.getNodeConf(node);
        if(conf.isEmpty()) {
            
            // Add the possibility to do disco info on foreign node.
            // Only available for local users.
            if(dataStore.isLocalUser(reqIQ.getFrom().toBareJID())) {
                
                // If we are here, it means we have a node that was not on this channel server
                // but the user was local. Let's try to find it from a foreign node.
                
                DiscoInfo di = DiscoInfo.buildDiscoInfoStatemachine(node, reqIQ, dataStore);
                outQueue.put(di.nextStep());
                return;
                
            }
            
            /*
                Not found. Let's return something like this:
                <iq type='error'
                    from='plays.shakespeare.lit'
                    to='romeo@montague.net/orchard'
                    id='info1'>
                  <query xmlns='http://jabber.org/protocol/disco#info' node='/user/pretty@lady.com/posts'/>
                  <error type='cancel'>
                    <item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                  </error>
                </iq>
             */
            
            IQ reply = IQ.createResultIQ(reqIQ);
            reply.setChildElement(reqIQ.getChildElement().createCopy());
            reply.setType(Type.error);
            PacketError pe = new PacketError(org.xmpp.packet.PacketError.Condition.item_not_found, 
                                             org.xmpp.packet.PacketError.Type.cancel);
            reply.setError(pe);
            outQueue.put(reply);
            return;
        }
        
        TreeMap<String,String> sorted_conf = new TreeMap<String, String>();
        
        DataForm x = new DataForm(DataForm.Type.result);
        x.addField("FORM_TYPE", null, FormField.Type.hidden).addValue("http://jabber.org/protocol/pubsub#meta-data");

        sorted_conf.putAll(conf);
        for (String key : sorted_conf.keySet()) {
            x.addField(key, null, null).addValue(sorted_conf.get(key));
        }
        
        Element query = result.setChildElement(ELEMENT_NAME, JabberDiscoInfo.NAMESPACE_URI);
        query.addAttribute("node", node);
        query.addElement("identity")
             .addAttribute("category", "pubsub")
             .addAttribute("type", "leaf");
        query.addElement("feature")
             .addAttribute("var", "http://jabber.org/protocol/pubsub");

        query.add(x.getElement());
        
        outQueue.put(result);
    }
    
}
