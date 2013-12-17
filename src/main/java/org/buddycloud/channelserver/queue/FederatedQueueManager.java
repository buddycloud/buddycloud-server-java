package org.buddycloud.channelserver.queue;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

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
import org.xbill.DNS.SRVRecord;
import org.xbill.DNS.Record;
import org.xbill.DNS.Type;
import org.xbill.DNS.Lookup;
import org.xbill.DNS.TextParseException;

public class FederatedQueueManager {
	private static final Logger logger = Logger
			.getLogger(FederatedQueueManager.class);

	public static final String NO_CHANNEL_SERVER = "NO_CHANNEL_SERVER";
	public static final String DISCO_ITEMS = "DISCO_ITEMS";
	public static final String DISCO_INFO = "DISCO_INFO";
	public static final String DISCOVERED = "DISCOVERED";

	public static final String IDENTITY_TYPE_CHANNELS = "channels";
	public static final String BUDDYCLOUD_SERVER = "buddycloud-server";

    public static final String SRV_PREFIX = "_buddycloud-server._tcp.";

	private int id = 1;

	private final ChannelsEngine component;
	private ConcurrentHashMap<String, String> discoveredServers = new ConcurrentHashMap<String, String>();
	private ConcurrentHashMap<String, String> remoteChannelDiscoveryStatus = new ConcurrentHashMap<String, String>();
	private ConcurrentHashMap<String, Integer> remoteServerItemsToProcess = new ConcurrentHashMap<String, Integer>();
	private ConcurrentHashMap<String, String> remoteServerInfoRequestIds = new ConcurrentHashMap<String, String>();
	private ConcurrentHashMap<String, List<Packet>> waitingStanzas = new ConcurrentHashMap<String, List<Packet>>();

	private ConcurrentHashMap<String, String> idMap = new ConcurrentHashMap<String, String>();

	private ExpiringPacketQueue sentRemotePackets = new ExpiringPacketQueue();
	private ExpiringPacketQueue nodeMap = new ExpiringPacketQueue();

	private String localServer;

	public FederatedQueueManager(ChannelsEngine component, String localServer) {
		this.component = component;
		this.localServer = localServer;

		nodeMap.start();
		sentRemotePackets.start();
	}

	private int getId() {
		int id = this.id;
		this.id++;
		return id;
	}

	public void process(Packet packet) throws ComponentException {

		logger.debug("Packet payload " + packet.toXML() + " going to federation.");

		String to = packet.getTo().toString();

		String uniqueId = generateUniqueId(packet);
		idMap.put(uniqueId, packet.getID());
		packet.setID(uniqueId);

		sentRemotePackets.put(uniqueId, packet.getFrom());
		try {
			extractNodeDetails(packet);
			// Do we have a map already?
			if (discoveredServers.containsKey(to)) {
				packet.setTo(new JID(discoveredServers.get(to)));
				sendPacket(packet.createCopy());
				return;
			}
			// Are we already discovering a remote server?
			if (!remoteChannelDiscoveryStatus.containsKey(to)) {
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
			// Add packet to list
			if (!waitingStanzas.containsKey(to)) {
				waitingStanzas.put(to, new ArrayList<Packet>());
			}
			waitingStanzas.get(to).add(packet);
			logger.debug("Adding packet to waiting stanza list for " + to
					+ " (size " + waitingStanzas.get(to).size() + ")");
		} catch (Exception e) {
			logger.error(e);
		}
	}

	private void extractNodeDetails(Packet packet) {
		try {
			String packetXml = packet.toXML();
			if (!packetXml.contains("node=")) {
				return;
			}
			nodeMap.put(
			    packet.getID(),
			    packetXml.split("node=\"")[1].split("\"")[0]
			);
		} catch (NullPointerException e) {
			logger.info("No node details found in federated packet");
			logger.error(e);
		} catch (ArrayIndexOutOfBoundsException e) {
			logger.info("Error extracting node information from federated packet");
			logger.error(e);
		}
	}

	private void sendPacket(Packet packet) throws ComponentException {
		packet.setFrom(localServer);
		component.sendPacket(packet.createCopy());
	}

	private void discoverRemoteChannelServer(String remoteDomain, String id)
			throws ComponentException {
		logger.info("Attemping to discover remote server " + remoteDomain);
		IQ discover = new IQ(IQ.Type.get);
		discover.setFrom(localServer);
		discover.setTo(remoteDomain);
		discover.setID(id);
		discover.getElement().addElement("query", JabberPubsub.NS_DISCO_ITEMS);
		component.sendPacket(discover);
		remoteChannelDiscoveryStatus.put(remoteDomain, DISCO_ITEMS);
	}

	public void processDiscoItemsResponse(JID from, List<Element> items)
			throws ComponentException {

		for (Element item : items) {
			Attribute name = item.attribute("name");
			if (name != null
					&& name.getStringValue().equals(BUDDYCLOUD_SERVER)) {
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

	public boolean isFederatedDiscoInfoRequest(String packetId) {
		return remoteServerInfoRequestIds.containsKey(packetId);
	}

	private void setDiscoveredServer(String server, String handler) {
		discoveredServers.put(server, handler);
	}

	public void processDiscoInfoResponse(JID from, String id,
			List<Element> identities) throws ComponentException {
		String originatingServer = remoteServerInfoRequestIds.get(id);
		if (originatingServer == null) {
			return;
		}
		remoteServerInfoRequestIds.remove(id);
		remoteServerItemsToProcess.put(originatingServer,
				remoteServerItemsToProcess.get(originatingServer) - 1);

		String identityType;
		for (Element identity : identities) {
			identityType = identity.attributeValue("type");
			if (identityType != null
					&& identityType.equals(IDENTITY_TYPE_CHANNELS)) {
				setDiscoveredServer(originatingServer, from.toString());
				sendFederatedRequests(originatingServer);
			}
		}

		if (remoteServerItemsToProcess.get(originatingServer) < 1) {
			if (!discoveredServers.containsKey(originatingServer)) {
                if (false == attemptDnsDiscovery(originatingServer)) {
                    sendRemoteChannelServerNotFoundErrorResponses(originatingServer);
                    remoteChannelDiscoveryStatus.put(originatingServer,
                            NO_CHANNEL_SERVER);
                    waitingStanzas.remove(originatingServer);
                }
			} else {
				remoteChannelDiscoveryStatus.put(originatingServer, DISCOVERED);
			}
		}
	}

    private boolean attemptDnsDiscovery(String originatingServer) throws ComponentException {
        try {
            String query = SRV_PREFIX + originatingServer;
            Record[] records = new Lookup(query, Type.SRV).run();
            if ((null == records) || (0 == records.length)) {
                return false;
            }
            SRVRecord record = (SRVRecord) records[0];
            setDiscoveredServer(originatingServer, record.getTarget().toString());
            sendFederatedRequests(originatingServer);
            logger.info("Used DNS fallback to discover buddycloud server for "
                + originatingServer + " (" + record.getTarget().toString() + ")");
            return true;
        } catch (TextParseException e) {
            logger.error(e);
            return false;
        }
    }

	private void sendFederatedRequests(String originatingServer)
			throws ComponentException {
		String remoteServer = discoveredServers.get(originatingServer);
		List<Packet> packetsToSend = waitingStanzas.get(originatingServer);
		if (packetsToSend == null) {
			return;
		}
		for (Packet packet : packetsToSend) {
			packet.setTo(remoteServer);
			sendPacket(packet.createCopy());
		}
		waitingStanzas.remove(originatingServer);
	}

	private void sendRemoteChannelServerNotFoundErrorResponses(String server)
			throws ComponentException {

		List<Packet> queued = waitingStanzas.get(server);
		if (queued == null) {
			return;
		}
		Element noRemoteServer = new DOMElement("text", new Namespace("",
				JabberPubsub.NS_PUBSUB_ERROR));
		noRemoteServer.setText("No pubsub channel service discovered for "
				+ server);
		Element itemNotFound = new DOMElement(
				PacketError.Condition.item_not_found.toXMPP(), new Namespace(
						"", JabberPubsub.NS_XMPP_STANZAS));
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
		if (!sentRemotePackets.containsKey(packet.getID())) {
			throw new UnknownFederatedPacketException(
					"Can not find original requesting packet! (ID:"
							+ packet.getID() + ")");
		}

		String uniqueId = packet.getID();
		packet.setID(idMap.get(uniqueId));
		packet.setTo((JID) sentRemotePackets.get(uniqueId));
		packet.setFrom(localServer);
		sentRemotePackets.remove(packet.getID());
		idMap.remove(uniqueId);

		component.sendPacket(packet);
	}

	public String getRelatedNodeForRemotePacket(IQ packet) {
		String id = null;
		if (nodeMap.containsKey(packet.getID())) {
			id = (String) nodeMap.get(packet.getID());
			nodeMap.remove(packet.getID());
		}
		return id;
	}

	public void addChannelMap(JID server) {
		setDiscoveredServer(server.getDomain(), server.getDomain());
		remoteChannelDiscoveryStatus.put(server.getDomain(), DISCOVERED);
		try {
			sendFederatedRequests(server.getDomain());
		} catch (ComponentException e) {
			e.printStackTrace();
			logger.error(e);
		}
	}

	/**
	 * Generate a unique ID for a packet
	 *
	 * Supplied packet IDs might not be unique so we use the ID and the FROM
	 * values to create a hash which we map back to the original packet ID.
	 *
	 * @param packet
	 * @return unique ID for the packet
	 */
	private String generateUniqueId(Packet packet) {
		return generateMd5(packet.getID() + packet.getFrom());
	}

	/**
	 * Generates an MD5 hash of a supplied String
	 *
	 * @param message to encode
	 * @return MD5 Hash of supplied string
	 */
    private String generateMd5(String message) {
        String digest = null;
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] hash = md.digest(message.getBytes("UTF-8"));

            //converting byte array to Hexadecimal String
            StringBuilder sb = new StringBuilder(2*hash.length);
            for(byte b : hash) {
               sb.append(String.format("%02x", b&0xff));
            }

            digest = sb.toString();
        } catch (UnsupportedEncodingException e) {
			logger.info("Error generating unique packet ID");
			logger.error(e);
        } catch (NoSuchAlgorithmException e) {
			logger.info("Error generating unique packet ID");
			logger.error(e);
        }
        return digest;
    }
}