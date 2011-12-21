package org.buddycloud.channelserver.packetHander.IQ.Namespace;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Properties;
import java.util.TreeMap;

import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.queue.AOutQueue;
import org.buddycloud.channelserver.queue.statemachine.DiscoInfo;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.PacketError;

public class JabberDiscoInfo extends AbstractNamespace {

	public static final String NAMESPACE_URI = "http://jabber.org/protocol/disco#info";
	
	public JabberDiscoInfo(AOutQueue outQueue, Properties conf, DataStore dataStore) {
		
		super(outQueue, conf, dataStore);
		getProcessors.put(GetQuery.ELEMENT_NAME, new GetQuery());
		
	}

	private class GetQuery implements IAction {

		public static final String ELEMENT_NAME = "query";
		
		@Override
		public void process() {
			
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
            
            HashMap<String, String> conf = dataStore.getNodeConf(node);
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
            
            // TODO, this shorting is crazy, it's only for the unit tests.
            //       man, should be fixed.
            ValueComparator bvc =  new ValueComparator(conf);
            TreeMap<String,String> sorted_conf = new TreeMap<String, String>(bvc);
            
            DataForm x = new DataForm(DataForm.Type.result);
            x.addField("FORM_TYPE", null, FormField.Type.hidden).addValue("http://jabber.org/protocol/pubsub#meta-data");

            sorted_conf.putAll(conf);
            for (String key : sorted_conf.keySet()) {
                x.addField(key, null, null).addValue(sorted_conf.get(key));
            }
            
            Element query = result.setChildElement(ELEMENT_NAME, NAMESPACE_URI);
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
	
	private class ValueComparator implements Comparator<Object> {

	    HashMap<String, String> base;
	    public ValueComparator(HashMap<String, String> base) {
	        this.base = base;
	    }
        @Override
        public int compare(Object o1, Object o2) {

            String a = (String) base.get(o1);
            String b = (String) base.get(o2);
            
            return a.compareTo(b);
        }
       
	  }
	
}
