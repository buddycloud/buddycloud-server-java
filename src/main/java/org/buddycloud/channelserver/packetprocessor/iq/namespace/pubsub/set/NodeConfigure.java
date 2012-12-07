package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.channel.node.configuration.field.Owner;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliation;
import org.buddycloud.channelserver.pubsub.event.Event;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSet;

public class NodeConfigure extends PubSubElementProcessorAbstract {
	protected String node;

	private static final Logger LOGGER = Logger.getLogger(NodeConfigure.class);

	public NodeConfigure(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		setChannelManager(channelManager);
		setOutQueue(outQueue);
	}

	public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm)
			throws Exception {
		element = elm;
		response = IQ.createResultIQ(reqIQ);
		request = reqIQ;
		actor = actorJID;
		node = element.attributeValue("node");

		if (null == actor) {
			actor = request.getFrom();
		}
		if (false == channelManager.isLocalNode(node)) {
			makeRemoteRequest();
			return;
		}
		try {
			if ((false == nodeProvided()) || (false == nodeExists())
					|| (false == userCanModify())) {
				outQueue.put(response);
				return;
			}
		} catch (NodeStoreException e) {
			setErrorCondition(PacketError.Type.cancel,
					PacketError.Condition.internal_server_error);
			outQueue.put(response);
			return;
		}
		setNodeConfiguration();
	}

	private void setNodeConfiguration() throws Exception {
		try {
			getNodeConfigurationHelper().parse(request);
			if (true == getNodeConfigurationHelper().isValid()) {
				HashMap<String, String> configuration = getNodeConfigurationHelper()
						.getValues();
				configuration
						.put(Owner.FIELD_NAME, channelManager.getNodeConfValue(
								node, Owner.FIELD_NAME));
				updateNodeConfiguration(configuration);
				notifySubscribers(configuration);
				return;
			}
		} catch (NodeConfigurationException e) {
			LOGGER.error("Node configuration exception", e);
			setErrorCondition(PacketError.Type.modify,
					PacketError.Condition.bad_request);
			outQueue.put(response);
			return;
		} catch (NodeStoreException e) {
			LOGGER.error("Data Store Exception", e);
			setErrorCondition(PacketError.Type.cancel,
					PacketError.Condition.internal_server_error);
			outQueue.put(response);
			return;
		}
		setErrorCondition(PacketError.Type.modify,
				PacketError.Condition.bad_request);
		outQueue.put(response);
	}

	private void updateNodeConfiguration(HashMap<String, String> configuration)
			throws Exception {
		channelManager.setNodeConf(node, configuration);
		outQueue.put(response);
	}

	private void notifySubscribers(HashMap<String, String> configuration) throws NodeStoreException,
			InterruptedException {
		ResultSet<NodeSubscription> subscribers = channelManager
				.getNodeSubscriptionListeners(node);
		Document document = getDocumentHelper();
		Element message = document.addElement("message");
		Element event = message.addElement("event");
		Element configurationElement = event.addElement("configuration");
		configurationElement.addAttribute("node", node);
		event.addNamespace("", Event.NAMESPACE);
		message.addAttribute("id", request.getID() + "-1");
		message.addAttribute("from", request.getTo().toString());
		message.addAttribute("type", "headline");
		Message rootElement = new Message(message);
		
		Element dataForm = configurationElement.addElement("x");
		DataForm df = new DataForm(dataForm);
		FormField field;
		for (Map.Entry<String, String> entry : configuration.entrySet()) {
		    String key = entry.getKey();
		    Object value = entry.getValue();
		    field = df.addField(key, null, null);
		    field.addValue(value);
		    // ...
		}	

		for (NodeSubscription subscriber : subscribers) {
			Message notification = rootElement.createCopy();
			notification.setTo(subscriber.getListener());
			outQueue.put(notification);
		}
	}

	private boolean userCanModify() throws NodeStoreException {
		String owner = channelManager.getNodeConfValue(node,
				Affiliation.OWNER.toString());

		if ((null != owner) && (true == owner.equals(actor.toBareJID()))) {
			return true;
		}
		setErrorCondition(PacketError.Type.auth,
				PacketError.Condition.forbidden);
		return false;
	}

	private boolean nodeExists() throws NodeStoreException {
		if (true == channelManager.nodeExists(node)) {
			return true;
		}
		setErrorCondition(PacketError.Type.cancel,
				PacketError.Condition.item_not_found);
		return false;
	}

	private boolean nodeProvided() {
		if ((null != node) && !node.equals("")) {
			return true;
		}
		response.setType(IQ.Type.error);
		Element nodeIdRequired = new DOMElement("nodeid-required",
				new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
		Element badRequest = new DOMElement(
				PacketError.Condition.bad_request.toXMPP(), new Namespace("",
						JabberPubsub.NS_XMPP_STANZAS));
		Element error = new DOMElement("error");
		error.addAttribute("type", "modify");
		error.add(badRequest);
		error.add(nodeIdRequired);
		response.setChildElement(error);
		return false;
	}
	
	private void makeRemoteRequest() throws InterruptedException {
		request.setTo(new JID(node.split("/")[2]).getDomain());
		Element actor = request.getElement()
		    .element("pubsub")
		    .addElement("actor", JabberPubsub.NS_BUDDYCLOUD);
		actor.addText(request.getFrom().toBareJID());
	    outQueue.put(request);
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("configure");
	}
}