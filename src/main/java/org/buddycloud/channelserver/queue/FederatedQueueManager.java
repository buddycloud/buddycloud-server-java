package org.buddycloud.channelserver.queue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.dom4j.Element;
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
		try {
			// Do we have a map already?
			if (discoveredServers.containsKey(to)) {
				packet.setTo(new JID(discoveredServers.get(to)));
				logger.debug("\n\n\n***** Already have a server for packet: " + packet.toXML());
				component.sendPacket(packet);
				return;
			}
			// Are we already discovering a remote server?
			if (false == remoteChannelDiscoveryStatus.containsKey(to)) {
				discoverRemoteChannelServer(to, packet.getID());
			} else if (remoteChannelDiscoveryStatus.get(to).equals(
					NO_CHANNEL_SERVER)) {
				logger.debug("\n\nNo remote channel server for " + to + "\n\n");
				IQ reply = IQ.createResultIQ((IQ) packet);
				reply.setError(new PacketError(
						PacketError.Condition.remote_server_not_found,
						PacketError.Type.cancel));
				component.sendPacket(reply);
				return;
			}
			// Add packet to list
			if (false == waitingStanzas.containsKey(to)) {
				waitingStanzas.put(to, new ArrayList<Packet>());
			}
			logger.debug("\n\nAdding packet to waiting stanza list for " 
			        + to + " (size " + waitingStanzas.get(to).size() + ")\n\n");
			waitingStanzas.get(to).add(packet);
		} catch (Exception e) {
			e.printStackTrace();
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

		IQ infoRequest = new IQ(IQ.Type.get);
		infoRequest.setFrom(localServer);
		infoRequest.getElement()
				.addElement("query", JabberPubsub.NS_DISCO_INFO);

		remoteServerItemsToProcess.put(from.toString(), items.size());
		String infoRequestId;
        String itemName;
		for (Element item : items) {
			itemName = item.attributeValue("name");
			if ((itemName != null) && (true == itemName.equals(BUDDYCLOUD_SERVER))) {
				discoveredServers.put(from.toString(), item.attributeValue("jid"));
			} else {
				infoRequestId = "info:" + getId();
				infoRequest.setTo(item.attributeValue("jid"));
				infoRequest.setID(infoRequestId);
				remoteServerInfoRequestIds.put(infoRequestId, from.toString());
				component.sendPacket(infoRequest.createCopy());
			}
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
			logger.debug("\n** Catching up on packet: " + packet.toString());
			component.sendPacket(packet);
		}
		waitingStanzas.remove(originatingServer);		
	}

	private void sendRemoteChannelServerNotFoundErrorResponses(String server) {
		// TODO: Send error responses		
	}
}