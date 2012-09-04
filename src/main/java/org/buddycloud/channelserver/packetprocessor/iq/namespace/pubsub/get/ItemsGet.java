package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.io.StringReader;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.db.jedis.NodeEntryImpl;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.entry.NodeEntry;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.buddycloud.channelserver.db.jedis.NodeSubscriptionImpl;
import org.buddycloud.channelserver.db.DataStoreException;

public class ItemsGet implements PubSubElementProcessor
{
    private static final Logger LOGGER = Logger.getLogger(ItemsGet.class);

	private static final int MAX_ITEMS_TO_RETURN = 50;
    
    private final BlockingQueue<Packet> outQueue;
    private final DataStore dataStore;
    
    private String    node;
    private String    firstItem;
    private String    lastItem;
    private SAXReader xmlReader;
    private Element   entry;
	private IQ        requestIq;
	private JID       fetchersJid;
    
    public ItemsGet(BlockingQueue<Packet> outQueue, DataStore dataStore)
    {
        this.outQueue = outQueue;
        this.dataStore = dataStore;
    }
    
    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception
    {    
        node      = elm.attributeValue("node");
        requestIq = reqIQ;
        
        if ((node == null) || (true == node.equals(""))) {
            missingJidRequest();
            return;
        }
        
        fetchersJid               = requestIq.getFrom();
        boolean isLocalNode       = dataStore.isLocalNode(node);
        boolean isLocalSubscriber = false;
		
        if (actorJID != null) {
            fetchersJid = actorJID;
        } else {
            isLocalSubscriber = dataStore.isLocalUser(fetchersJid.toBareJID());
        }
        if (!isLocalNode) {
            if (isLocalSubscriber) {
                
                //TODO, WORK HERE!
                
                // Start process to fetch items from nodes.
                //Subscribe sub = Subscribe.buildSubscribeStatemachine(node, requestIq, dataStore);
                //outQueue.put(sub.nextStep());
                //return;
            }
            
            // Foreign client is trying to fetch items of a node that does not exists.
            /*
                6.1.3.12 Node Does Not Exist
            
                <iq type='error'
                        from='pubsub.shakespeare.lit'
                        to='francisco@denmark.lit/barracks'
                        id='sub1'>
                      <error type='cancel'>
                        <item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                      </error>
                </iq>
             */
            IQ reply = IQ.createResultIQ(requestIq);
            reply.setType(Type.error);
            PacketError pe = new PacketError(
                org.xmpp.packet.PacketError.Condition.item_not_found, 
                org.xmpp.packet.PacketError.Type.cancel
            );
            reply.setError(pe);
            outQueue.put(reply);
            return;
        }
        
        NodeSubscriptionImpl nodeSubscription = dataStore.getUserSubscriptionOfNode(
            fetchersJid.toBareJID(), 
            node
        );
        String possibleExistingAffiliation  = nodeSubscription.getAffiliation();
        String possibleExistingSusbcription = nodeSubscription.getSubscription();
        
        // TODO, add here ACL checks
        
        Element pubsub = new DOMElement(
        	PubSubGet.ELEMENT_NAME,
            new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI)
        );
        
        int maxItemsToReturn = MAX_ITEMS_TO_RETURN;
        String afterItemId   = null;
        
        String max_items = elm.attributeValue("max_items");
        if (max_items != null) {
            maxItemsToReturn = Integer.parseInt(max_items);
        }
        
        //Requests
        //<set xmlns='http://jabber.org/protocol/rsm'>
        //  <max>10</max>
        //</set>
        //
        // <set xmlns='http://jabber.org/protocol/rsm'>
        //  <max>10</max>
        //  <after>peterpan@neverland.lit</after>
        //</set>
        if (rsm != null) {
            Element max = rsm.element("max");
            if (max != null) {
                maxItemsToReturn = Integer.parseInt(max.getTextTrim());
            }
            Element after = rsm.element("after");
            if (after != null) {
                afterItemId = after.getTextTrim();
            }
        }
        
        Element items = pubsub.addElement("items");
        items.addAttribute("node", node);
        
        xmlReader             = new SAXReader();
        entry                 = null;
        int totalEntriesCount = 0;

        if (node.substring(node.length() - 13).equals("subscriptions")) {
        	totalEntriesCount = getSubscriptionItems(items, maxItemsToReturn, afterItemId);
        } else {
        	totalEntriesCount = getNodeItems(items, maxItemsToReturn, afterItemId);
        }
        
        if ((rsm != null) || (totalEntriesCount > maxItemsToReturn)) {
            /* 
               TODO, add result set here as defined in 6.5.4 Returning Some Items
               <set xmlns='http://jabber.org/protocol/rsm'>
                  <first index='0'>368866411b877c30064a5f62b917cffe</first>
                  <last>4e30f35051b7b8b42abe083742187228</last>
                  <count>19</count>
               </set>
            */
            Element resultSetMangement = pubsub.addElement("set", "http://jabber.org/protocol/rsm");
            
            if (firstItem != null) {
                resultSetMangement.addElement("first").setText(firstItem);
                resultSetMangement.addElement("last").setText(lastItem);
            }
            resultSetMangement.addElement("count").setText(Integer.toString(totalEntriesCount));
        }
        
        IQ result = IQ.createResultIQ(requestIq);
        result.setChildElement(pubsub);
        
        outQueue.put(result);     
    }

    private int getNodeItems(Element items, int maxItemsToReturn, String afterItemId)
    {
    	try {
    	    Iterator<? extends NodeEntry> cur = dataStore.getNodeEntries(node, maxItemsToReturn, afterItemId);
    	
	        while (cur.hasNext()) {
	            NodeEntryImpl ne = (NodeEntryImpl) cur.next();
	            Element item     = items.addElement("item");
	            item.addAttribute("id", ne.getId());
	            
	            if (firstItem == null) {
	                firstItem = ne.getMongoId();
	            }
	            try {
	                entry = xmlReader.read(new StringReader(ne.getEntry())).getRootElement();
	                item.add(entry);
	                lastItem = ne.getMongoId();
	            } catch (DocumentException e) {
	                LOGGER.error("Something is wrong when reading the items from a node '" + node + "'!");
	            }
	        }
	        return dataStore.getNodeEntriesCount(node);
    	} catch (DataStoreException e) {
    		e.printStackTrace();
    		return 0;
    	}
	}
    
    private int getSubscriptionItems(Element items, int maxItemsToReturn, String afterItemId)
    {
    	try {
	    	Iterator<? extends NodeSubscription> subscribers = dataStore.getNodeSubscribers(node);
	    	int entries = 0;
	    	Element jidItem;
	    	Element query;
	    	
	    	while (subscribers.hasNext()) {
	    		NodeSubscriptionImpl subscriber = (NodeSubscriptionImpl) subscribers.next();
	    		
	    		jidItem = items.addElement("item");
	    		jidItem.addAttribute("id", subscriber.getBareJID());
	    		query = jidItem.addElement("query");
		    	query.addNamespace("", JabberPubsub.NS_DISCO_ITEMS);

	    		if (firstItem == null) {
	                firstItem = subscriber.getBareJID();
	            }
	    		lastItem = subscriber.getBareJID();
	    		addSubscriptionItems(query, subscriber.getBareJID());
	    		entries++;
	    	}
	    	return entries;
    	} catch (DataStoreException e) {
    		e.printStackTrace();
    		return 0;
    	}
    }
    
    private void addSubscriptionItems(Element query, String subscriber)
    {
    	Iterator<? extends NodeSubscription> subscriptions = dataStore
    	        .findUserSubscriptionOfNodes(fetchersJid.toBareJID(), subscriber);
    	Element item;
		while (subscriptions.hasNext()) {
			NodeSubscriptionImpl subscription = (NodeSubscriptionImpl) subscriptions.next();
			item = query.addElement("item");
			item.addNamespace("ns1", JabberPubsub.NAMESPACE_URI);
			item.addNamespace("ns2", JabberPubsub.NAMESPACE_URI);
			item.addAttribute("jid", fetchersJid.toBareJID());
    		item.addAttribute("node", subscription.getNode());
    		item.addAttribute("ns1:affiliation", subscription.getAffiliation());
    		item.addAttribute("ns2:subscription", subscription.getSubscription());
		}
    }

	private void missingJidRequest() throws InterruptedException
    {
        /*
            7.2.3.3 NodeID Required
            
            <iq type='error'
                from='pubsub.shakespeare.lit'
                to='hamlet@denmark.lit/elsinore'
                id='retract1'>
              <error type='modify'>
                <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
                <nodeid-required xmlns='http://jabber.org/protocol/pubsub#errors'/>
              </error>
            </iq>
        */
        IQ reply = IQ.createResultIQ(requestIq);
        reply.setType(Type.error);
        
        Element badRequest = new DOMElement(
            "bad-request",
            new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS)
        );

        Element nodeIdRequired = new DOMElement(
            "nodeid-required",
            new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR)
        );
        
        Element error = new DOMElement("error");
        error.addAttribute("type", "modify");
        error.add(badRequest);
        error.add(nodeIdRequired);
        
        reply.setChildElement(error);
        outQueue.put(reply);
	}

    public boolean accept(Element elm)
	{
        return elm.getName().equals("items");
    }
}