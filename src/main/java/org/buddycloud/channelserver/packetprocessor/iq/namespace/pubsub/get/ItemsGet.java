package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.ChannelNodeRef;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.dom.DOMElement;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.packet.PacketError.Type;

public class ItemsGet implements PubSubElementProcessor {
	private static final Logger LOGGER = Logger.getLogger(ItemsGet.class);

	private static final int MAX_ITEMS_TO_RETURN = 50;

	private final BlockingQueue<Packet> outQueue;

	private ChannelManager channelManager;
	private String node;
	private String firstItem;
	private String lastItem;
	private SAXReader xmlReader;
	private Element entry;
	private IQ requestIq;
	private JID fetchersJid;
	private IQ reply;
	private Element resultSetManagement;
	private Element element;

	private NodeViewAcl nodeViewAcl;
	private Map<String, String> nodeDetails;

	public ItemsGet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
		this.outQueue = outQueue;
		setChannelManager(channelManager);
	}

	public void setChannelManager(ChannelManager ds) {
		channelManager = ds;
	}

	public void setNodeViewAcl(NodeViewAcl acl) {
		nodeViewAcl = acl;
	}

	private NodeViewAcl getNodeViewAcl() {
		if (null == nodeViewAcl) {
			nodeViewAcl = new NodeViewAcl();
		}
		return nodeViewAcl;
	}

	@Override
	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		node = elm.attributeValue("node");
		requestIq = reqIQ;
		reply = IQ.createResultIQ(reqIQ);
		element = elm;
		resultSetManagement = rsm;

		if ((node == null) || (true == node.equals(""))) {
			missingJidRequest();
			outQueue.put(reply);
			return;
		}

		fetchersJid = requestIq.getFrom();

		try {
			if(!channelManager.nodeExists(node)) {
				setErrorCondition(PacketError.Type.cancel,
						PacketError.Condition.item_not_found);

				outQueue.put(reply);
				return;
			}
			
			// boolean isLocalNode = channelManager.isLocalNode(node);
			// boolean isLocalSubscriber = false;

			if (actorJID != null) {
				fetchersJid = actorJID;
			} else {
				// isLocalSubscriber =
				// channelManager.isLocalUser(fetchersJid.toBareJID());
			}
			/*
			 * if (!isLocalNode) { handleForeignNode(isLocalSubscriber); return;
			 * }
			 */
			if (false == userCanViewNode()) {
				outQueue.add(reply);
				return;
			}
			getItems();
		} catch (NodeStoreException e) {
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
		}
		outQueue.put(reply);
	}

	private void setErrorCondition(Type type, Condition condition) {
		reply.setType(IQ.Type.error);
		PacketError error = new PacketError(condition, type);
		reply.setError(error);
	}

	private void getItems() throws Exception {
		Element pubsub = new DOMElement(PubSubGet.ELEMENT_NAME,
				new org.dom4j.Namespace("", JabberPubsub.NAMESPACE_URI));

		int maxItemsToReturn = MAX_ITEMS_TO_RETURN;
		String afterItemId = null;

		String max_items = element.attributeValue("max_items");
		if (max_items != null) {
			maxItemsToReturn = Integer.parseInt(max_items);
		}

		// Requests
		// <set xmlns='http://jabber.org/protocol/rsm'>
		// <max>10</max>
		// </set>
		//
		// <set xmlns='http://jabber.org/protocol/rsm'>
		// <max>10</max>
		// <after>peterpan@neverland.lit</after>
		// </set>
		if (resultSetManagement != null) {
			Element max = resultSetManagement.element("max");
			if (max != null) {
				maxItemsToReturn = Integer.parseInt(max.getTextTrim());
			}
			Element after = resultSetManagement.element("after");
			if (after != null) {
				afterItemId = after.getTextTrim();
			}
		}

		Element items = pubsub.addElement("items");
		items.addAttribute("node", node);

		xmlReader = new SAXReader();
		entry = null;
		int totalEntriesCount = 0;

		if (node.substring(node.length() - 13).equals("subscriptions")) {
			totalEntriesCount = getSubscriptionItems(items, maxItemsToReturn,
					afterItemId);
		} else {
			totalEntriesCount = getNodeItems(items, maxItemsToReturn,
					afterItemId);
		}

		if ((resultSetManagement != null)
				|| (totalEntriesCount > maxItemsToReturn)) {
			/*
			 * TODO, add result set here as defined in 6.5.4 Returning Some
			 * Items <set xmlns='http://jabber.org/protocol/rsm'> <first
			 * index='0'>368866411b877c30064a5f62b917cffe</first>
			 * <last>4e30f35051b7b8b42abe083742187228</last> <count>19</count>
			 * </set>
			 */
			Element rsm = pubsub.addElement("set",
					"http://jabber.org/protocol/rsm");

			if (firstItem != null) {
				rsm.addElement("first").setText(firstItem);
				rsm.addElement("last").setText(lastItem);
			}
			rsm.addElement("count")
					.setText(Integer.toString(totalEntriesCount));
		}

		reply.setChildElement(pubsub);
	}

	private boolean userCanViewNode() throws NodeStoreException {
		NodeSubscription nodeSubscription = channelManager.getUserSubscription(node, fetchersJid);
		NodeAffiliation nodeAffiliation = channelManager.getUserAffiliation(node, fetchersJid);
		
		
		
		Affiliations possibleExistingAffiliation = Affiliations.none;
		Subscriptions possibleExistingSubscription = Subscriptions.none;
        if (null != nodeSubscription) {
			if (null != nodeAffiliation.getAffiliation()) {
				possibleExistingAffiliation = nodeAffiliation.getAffiliation();
			}
			if (null != nodeSubscription.getSubscription()) {
				possibleExistingSubscription = nodeSubscription.getSubscription();
			}
        }
		if (true == getNodeViewAcl().canViewNode(node,
				possibleExistingAffiliation, possibleExistingSubscription,
				getNodeAccessModel())) {
			return true;
		}
		NodeAclRefuseReason reason = getNodeViewAcl().getReason();
		createExtendedErrorReply(reason.getType(), reason.getCondition(),
				reason.getAdditionalErrorElement());
		return false;
	}

	private String getNodeAccessModel() {
		if (false == nodeDetails.containsKey(AccessModel.FIELD_NAME)) {
			return AccessModels.authorize.toString();
		}
		return nodeDetails.get(AccessModel.FIELD_NAME);
	}

	private void handleForeignNode(boolean isLocalSubscriber)
			throws InterruptedException {
		if (isLocalSubscriber) {

			// TODO, WORK HERE!

			// Start process to fetch items from nodes.
			// Subscribe sub = Subscribe.buildSubscribeStatemachine(node,
			// requestIq, channelManager);
			// outQueue.put(sub.nextStep());
			// return;
		}

		// Foreign client is trying to fetch items of a node that does not
		// exists.
		/*
		 * 6.1.3.12 Node Does Not Exist
		 * 
		 * <iq type='error' from='pubsub.shakespeare.lit'
		 * to='francisco@denmark.lit/barracks' id='sub1'> <error type='cancel'>
		 * <item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
		 * </error> </iq>
		 */
		IQ reply = IQ.createResultIQ(requestIq);
		reply.setType(IQ.Type.error);
		PacketError pe = new PacketError(
				org.xmpp.packet.PacketError.Condition.item_not_found,
				org.xmpp.packet.PacketError.Type.cancel);
		reply.setError(pe);
		outQueue.put(reply);
		return;
	}

	private int getNodeItems(Element items, int maxItemsToReturn,
			String afterItemId) throws NodeStoreException {

		CloseableIterator<NodeItem> itemIt = channelManager.getNodeItems(node, afterItemId, maxItemsToReturn);

		try {
			while (itemIt.hasNext()) {
				NodeItem nodeItem = itemIt.next();
	
				if (firstItem == null) {
					firstItem = nodeItem.getId();
				}
				try {
					entry = xmlReader.read(new StringReader(nodeItem.getPayload()))
							.getRootElement();
					Element item = items.addElement("item");
					item.addAttribute("id", nodeItem.getId());
					item.add(entry);
					lastItem = nodeItem.getId();
				} catch (DocumentException e) {
					LOGGER.error("Error parsing a node entry, ignoring. "
							+ nodeItem);
				}
	
			}
			return channelManager.countNodeItems(node);
		} finally {
			if(itemIt != null) itemIt.close();
		}
	}

	/*
	    <item id="koski@buddycloud.com">
          <query xmlns="http://jabber.org/protocol/disco#items" xmlns:pubsub="http://jabber.org/protocol/pubsub" xmlns:atom="http://www.w3.org/2005/Atom">
            <item jid="sandbox.buddycloud.com"
                  node="/user/koski@buddycloud.com/posts"
                  pubsub:affiliation="publisher">
              <atom:updated>2010-12-26T17:30:00Z</atom:updated>
            </item>
            <item jid="sandbox.buddycloud.com"
                  node="/user/koski@buddycloud.com/geo/future"/>
            <item jid="sandbox.buddycloud.com"
                  node="/user/koski@buddycloud.com/geo/current"/>
            <item jid="sandbox.buddycloud.com"
                  node="/user/koski@buddycloud.com/geo/previous"/>
            <item jid="sandbox.buddycloud.com"
                  node="/user/koski@buddycloud.com/mood"
                  pubsub:affiliation="member"/>
          </query>
        </item>
	 */
	private int getSubscriptionItems(Element items, int maxItemsToReturn,
			String afterItemId) throws NodeStoreException {
		ChannelNodeRef nodeRef = ChannelNodeRef.fromNodeId(node);
		
		Collection<NodeSubscription> subscribers = channelManager
				.getUserSubscriptions(nodeRef.getJID());
		int entries = 0;
		if (null == subscribers) {
			return entries;
		}
		Element jidItem;
		Element query;
		
		// We want to build up a list of all the followed channel jids, and a list of node subscriptions for each of them
		TreeMap<String,ArrayList<NodeSubscription>> followees = new TreeMap<String,ArrayList<NodeSubscription>>();
		
		for(NodeSubscription subscriber : subscribers) {
			// Only subscribed
			if(!subscriber.getSubscription().equals(Subscriptions.subscribed)) {
				continue;
			}

			ChannelNodeRef followedRef = ChannelNodeRef.fromNodeId(subscriber.getNodeId());
			
			ArrayList<NodeSubscription> subs = followees.get(followedRef.getJID().toString());
			
			if(subs == null) {
				subs = new ArrayList<NodeSubscription>();
				followees.put(followedRef.getJID().toString(), subs);
			}
			
			subs.add(subscriber);
		}		
		
		// If afterItemId is set then we will skip the results until we reach the id
		boolean skip = false;
		
		if(afterItemId != null) {
			skip = true;
		}
		
		for(Entry<String,ArrayList<NodeSubscription>> entry : followees.entrySet()) {
			if(skip) {
				if(entry.getKey().equals(afterItemId)) {
					skip = false;
				}
				continue;
			}
			
			jidItem = items.addElement("item");
			jidItem.addAttribute("id", entry.getKey());
			query = jidItem.addElement("query");
			query.addNamespace("", JabberPubsub.NS_DISCO_ITEMS);
			query.addNamespace("pubsub", JabberPubsub.NAMESPACE_URI);

			if (firstItem == null) {
				firstItem = entry.getKey();
			}
			lastItem = entry.getKey();
			addSubscriptionItems(query, entry.getValue());
			entries++;
			
			if((maxItemsToReturn > -1) && (entries > maxItemsToReturn)) {
				break;
			}
		}
		return subscribers.size();
	}

	private void addSubscriptionItems(Element query, List<NodeSubscription> subscriptions) throws NodeStoreException {
		Element item;
		
		if (null == subscriptions) {
			return;
		}
		
		for(NodeSubscription subscription : subscriptions) {
			NodeAffiliation affiliation = channelManager.getUserAffiliation(subscription.getNodeId(), subscription.getUser());
			
			item = query.addElement("item");
			item.addAttribute("jid", fetchersJid.toBareJID());
			item.addAttribute("node", subscription.getNodeId());
			item.addAttribute("pubsub:affiliation", affiliation.getAffiliation().toString());
			item.addAttribute("pubsub:subscription",
					subscription.getSubscription().toString());
		}
	}

	private void missingJidRequest() {
		createExtendedErrorReply(PacketError.Type.modify,
				PacketError.Condition.bad_request, "nodeid-required");
	}

	private void createExtendedErrorReply(Type type, Condition condition,
			String additionalElement) {
		reply.setType(IQ.Type.error);
		Element standardError = new DOMElement(condition.toString(),
				new org.dom4j.Namespace("", JabberPubsub.NS_XMPP_STANZAS));
		Element extraError = new DOMElement(additionalElement,
				new org.dom4j.Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
		Element error = new DOMElement("error");
		error.addAttribute("type", type.toString());
		error.add(standardError);
		error.add(extraError);
		reply.setChildElement(error);
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("items");
	}
}