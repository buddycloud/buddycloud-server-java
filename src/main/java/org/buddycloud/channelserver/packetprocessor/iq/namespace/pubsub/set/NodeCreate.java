package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.set;

import java.util.HashMap;
import java.util.concurrent.BlockingQueue;

import org.apache.bcel.generic.ACONST_NULL;
import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.channel.node.configuration.NodeConfigurationException;
import org.buddycloud.channelserver.channel.node.configuration.field.Owner;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeCreate extends PubSubElementProcessorAbstract {
	private static final String NODE_REG_EX = "^/user/[^@]+@[^/]+/[^/]+$";
	private static final String INVALID_NODE_CONFIGURATION = "Invalid node configuration";

	private static final Logger LOGGER = Logger.getLogger(NodeCreate.class);

	public NodeCreate(BlockingQueue<Packet> outQueue,
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

		if (null == actorJID) {
			actor = request.getFrom();
		}
		if (false == channelManager.isLocalNode(node)) {
			makeRemoteRequest();
			return;
		}
		if ((false == validateNode()) || (true == doesNodeExist())
				|| (false == actorIsRegistered())
				|| (false == nodeHandledByThisServer())) {
			outQueue.put(response);
			return;
		}
		createNode();
	}

	private void createNode() throws InterruptedException {
		try {
			channelManager.createNode(actor, node, getNodeConfiguration());
		} catch (NodeStoreException e) {
			logger.error(e);
			setErrorCondition(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
			outQueue.put(response);
			return;
		} catch (NodeConfigurationException e) {
			logger.error(e);
			setErrorCondition(PacketError.Type.modify,
					PacketError.Condition.bad_request);
			outQueue.put(response);
			return;
		}
		response.setType(IQ.Type.result);
		outQueue.put(response);
	}

	public boolean accept(Element elm) {
		return elm.getName().equals("create");
	}

	private HashMap<String, String> getNodeConfiguration() {
		getNodeConfigurationHelper().parse(request);
		if (false == getNodeConfigurationHelper().isValid()) {
			throw new NodeConfigurationException(INVALID_NODE_CONFIGURATION);
		}
		HashMap<String, String> defaultConfiguration = Conf.getDefaultChannelConf(
				new JID(node.split("/")[2]), actor);
		HashMap<String, String> configuration = getNodeConfigurationHelper()
				.getValues();
		configuration.put(Owner.FIELD_NAME, actor.toBareJID());
		
		defaultConfiguration.putAll(configuration);
		
		return defaultConfiguration;
	}

	private boolean validateNode() {
		if ((node != null) && !node.trim().equals("")) {
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

	private boolean doesNodeExist() throws NodeStoreException {
		if (false == channelManager.nodeExists(node)) {
			return false;
		}
		setErrorCondition(PacketError.Type.cancel,
				PacketError.Condition.conflict);
		return true;
	}

	private boolean actorIsRegistered() {
		if (true == actor.getDomain().equals(getServerDomain())) {
			return true;
		}
		setErrorCondition(PacketError.Type.auth,
				PacketError.Condition.forbidden);
		return false;
	}

	private boolean nodeHandledByThisServer() {
		if (false == node.matches(NODE_REG_EX)) {
			setErrorCondition(PacketError.Type.modify,
					PacketError.Condition.bad_request);
			return false;
		}

		if ((false == node.contains("@" + getServerDomain()))
				&& (false == node.contains("@" + getTopicsDomain()))) {
			setErrorCondition(PacketError.Type.modify,
					PacketError.Condition.not_acceptable);
			return false;
		}
		return true;
	}
	
	private void makeRemoteRequest() throws InterruptedException {
		request.setTo(new JID(node.split("/")[2]).getDomain());
		Element actor = request.getElement()
		    .element("pubsub")
		    .addElement("actor", JabberPubsub.NS_BUDDYCLOUD);
		actor.addText(request.getFrom().toBareJID());
	    outQueue.put(request);
	}
}