package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get;

import java.util.Map;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubGet;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class NodeConfigureGet extends PubSubElementProcessorAbstract {
	protected String node;

	private static final Logger LOGGER = Logger.getLogger(NodeConfigureGet.class);

	public NodeConfigureGet(BlockingQueue<Packet> outQueue,
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
		
		if (!nodeProvided()) {
			outQueue.put(response);
			return;
		}
		
		if (!channelManager.isLocalNode(node)) {
			makeRemoteRequest();
			return;
		}
		
		try {
			if (!nodeExists()) {
				outQueue.put(response);
				return;
			}
		} catch (NodeStoreException e) {
			LOGGER.error(e);
			setErrorCondition(PacketError.Type.cancel,
					PacketError.Condition.internal_server_error);
			outQueue.put(response);
			return;
		}
		getNodeConfiguration();
	}

	private void getNodeConfiguration() throws Exception {
		Map<String, String> nodeConf = channelManager.getNodeConf(node);
		
		DataForm x = new DataForm(DataForm.Type.result);

		FormField formType = x.addField();
		formType.setType(FormField.Type.hidden);
		formType.setVariable("FORM_TYPE");
		formType.addValue("http://jabber.org/protocol/pubsub#node_config");

		for (String key : nodeConf.keySet()) {
			x.addField(key, null, null).addValue(nodeConf.get(key));
		}
		Element pubsub = response.setChildElement(PubSubGet.ELEMENT_NAME, JabberPubsub.NS_PUBSUB_OWNER);
		Element configure = pubsub.addElement("configure");
		configure.addAttribute("node", node);
		configure.add(x.getElement());
		outQueue.put(response);
	}
	
	private boolean nodeExists() throws NodeStoreException {
		if (channelManager.nodeExists(node)) {
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