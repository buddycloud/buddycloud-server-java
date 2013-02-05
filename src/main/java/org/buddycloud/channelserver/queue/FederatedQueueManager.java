package org.buddycloud.channelserver.queue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.dom4j.Attribute;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.dom.DOMElement;
import org.xmpp.component.ComponentException;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class FederatedQueueManager {
	private static final Logger logger = Logger
			.getLogger(FederatedQueueManager.class);

	public static final String NO_CHANNEL_SERVER = "NO_CHANNEL_SERVER";
	public static final String DISCO_ITEMS = "DISCO_ITEMS";
	public static final String DISCO_INFO = "DISCO_INFO";
	public static final String DISCOVERED = "DISCOVERED";

	public static final Object IDENTITY_TYPE_CHANNELS = "channels";
	public static final Object BUDDYCLOUD_SERVER = "buddycloud-server";

	private int id = 1;

	private final ChannelsEngine component;
	private HashMap<String, String> discoveredServers = new HashMap<String, String>();
	private HashMap<String, List<Packet>> waitingStanzas = new HashMap<String, List<Packet>>();
	private HashMap<String, String> remoteChannelDiscoveryStatus = new HashMap<String, String>();
	private HashMap<String, Integer> remoteServerItemsToProcess = new HashMap<String, Integer>();
	private HashMap<String, String> remoteServerInfoRequestIds = new HashMap<String, String>();
	private HashMap<String, JID> sentRemotePackets = new HashMap<String, JID>();

	private String localServer;

	public FederatedQueueManager(ChannelsEngine component, String localServer) {
		this.component = component;
		this.localServer = localServer;
	}

	private int getId() {
		int id = this.id;
		this.id++;
		return id;
	}

	public void process(Packet packet) throws ComponentException {
		String to = packet.getTo().toString();
		sentRemotePackets.put(packet.getID(), packet.getFrom());
		packet.setFrom(localServer);
		try {
			// Do we have a map already?
			if (discoveredServers.containsKey(to)) {
				packet.setTo(new JID(discoveredServers.get(to)));
				component.sendPacket(packet);
				return;
			}
			// Are we already discovering a remote server?
			if (false == remoteChannelDiscoveryStatus.containsKey(to)) {
				discoverRemoteChannelServer(to, packet.getID());
			} else if (remoteChannelDiscoveryStatus.get(to).equals(
					NO_CHANNEL_SERVER)) {
				logger.error("No remote channel server for " + to);
				IQ reply = IQ.createResultIQ((IQ) packet);
				reply.setError(new PacketError(
						PacketError.Condition.remote_server_not_found,
						PacketError.Type.cancel));
				component.sendPacket(reply);
				return;
			}
			/* TODO: Handle no remote XMPP server
			 * 
			 * <iq xmlns='jabber:client' type='error' to='romeo@server1.com/client' 
			 *     from='channels.server1.com' id='1:items'>
			 *     <error type='cancel'>
			 *         <text>timeout</text>
			 *     </error>
			 * </iq>
			 */
			// Add packet to list
			if (false == waitingStanzas.containsKey(to)) {
				waitingStanzas.put(to, new ArrayList<Packet>());
			}
			logger.debug("Adding packet to waiting stanza list for " 
			        + to + " (size " + waitingStanzas.get(to).size() + ")");
			waitingStanzas.get(to).add(packet);
		} catch (Exception e) {
			logger.error(e);
		}
	}

	private void discoverRemoteChannelServer(String remoteDomain, String id)
			throws ComponentException {
		IQ discover = new IQ(IQ.Type.get);
		discover.setFrom(localServer);
		discover.setTo(remoteDomain);
		discover.setID(id);
		discover.getElement().addElement("query", JabberPubsub.NS_DISCO_ITEMS);
		component.sendPacket(discover);
		remoteChannelDiscoveryStatus.put(remoteDomain, DISCO_ITEMS);
	}

	public void sendInfoRequests(JID from, List<Element> items)
			throws ComponentException {

		for (Element item : items) {
            Attribute name = item.attribute("name");
			if ((null != name) && (true == name.getStringValue().equals(BUDDYCLOUD_SERVER))) {
				remoteChannelDiscoveryStatus.put(from.toString(), DISCOVERED);
				setDiscoveredServer(from.toString(), item.attributeValue("jid"));
				sendFederatedRequests(from.toString());
				return;
			}
		}
		IQ infoRequest = new IQ(IQ.Type.get);
		infoRequest.setFrom(localServer);
		infoRequest.getElement()
				.addElement("query", JabberPubsub.NS_DISCO_INFO);

		remoteServerItemsToProcess.put(from.toString(), items.size());
		String infoRequestId;
		for (Element item : items) {
			infoRequestId = getId() + ":info";
			infoRequest.setTo(item.attributeValue("jid"));
			infoRequest.setID(infoRequestId);
			remoteServerInfoRequestIds.put(infoRequestId, from.toString());
			component.sendPacket(infoRequest.createCopy());
		}
		remoteChannelDiscoveryStatus.put(from.toString(), DISCO_INFO);
	}

	public void processInfoResponses(JID from, String id,
			List<Element> identities) throws ComponentException {
		String originatingServer = remoteServerInfoRequestIds.get(id);
		remoteServerInfoRequestIds.remove(id);
		remoteServerItemsToProcess.put(originatingServer,
				remoteServerItemsToProcess.get(originatingServer) - 1);
		
		String identityType;
		for (Element identity : identities) {
			identityType = identity.attributeValue("type");
			if ((identityType != null)
					&& (true == identityType.equals(IDENTITY_TYPE_CHANNELS))) {
				discoveredServers.put(originatingServer, from.toString());
				sendFederatedRequests(originatingServer);
			}
		}
		if (remoteServerItemsToProcess.get(originatingServer) < 1) {
			remoteServerItemsToProcess.remove(originatingServer);
			if (false == discoveredServers.containsKey(originatingServer)) {
			    sendRemoteChannelServerNotFoundErrorResponses(originatingServer);
			    remoteChannelDiscoveryStatus.put(originatingServer, NO_CHANNEL_SERVER);
			    waitingStanzas.remove(originatingServer);
			} else {
				remoteChannelDiscoveryStatus.put(originatingServer, DISCOVERED);
			}
		}
	}

	private void sendFederatedRequests(String originatingServer) throws ComponentException {
		String remoteServer = discoveredServers.get(originatingServer);
		List<Packet> packetsToSend = waitingStanzas.get(originatingServer);
		if (null == packetsToSend) {
			return;
		}
		for (Packet packet : packetsToSend) {
			packet.setTo(remoteServer);
			component.sendPacket(packet);
		}
		waitingStanzas.remove(originatingServer);		
	}

	private void sendRemoteChannelServerNotFoundErrorResponses(String server)
			throws ComponentException {
		
        List<Packet> queued = waitingStanzas.get(server);
        if (null == queued) {
        	return;
        }
        Element noRemoteServer = new DOMElement("text",
				new Namespace("", JabberPubsub.NS_PUBSUB_ERROR));
        noRemoteServer.setText("No pubsub channel service discovered for " + server);
		Element itemNotFound = new DOMElement(
				PacketError.Condition.item_not_found.toXMPP(), new Namespace("",
						JabberPubsub.NS_XMPP_STANZAS));
		Element error = new DOMElement("error");
		error.addAttribute("type", PacketError.Type.cancel.toXMPP());
		error.add(itemNotFound);
		error.add(noRemoteServer);
        IQ response;
        for (Packet packet : queued) {
        	response = IQ.createResultIQ((IQ) packet);
        	response.setFrom(localServer);
        	response.setType(IQ.Type.error);
    		response.setChildElement(error);
    		component.sendPacket(response);
        }
	}

	public void passResponseToRequester(IQ packet) throws Exception {
		if (false == sentRemotePackets.containsKey(packet.getID())) {
			throw new UnknownFederatedPacketException(
			    "Can not find original requesting packet! (ID:" + packet.getID() + ")"
			);
		}
		packet.setTo(sentRemotePackets.get(packet.getID()));
		packet.setFrom(localServer);
		sentRemotePackets.remove(packet.getID());
		component.sendPacket(packet);
	}
}