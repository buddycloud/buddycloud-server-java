package org.buddycloud.channelserver.packetprocessor.iq.namespace.register;

import java.util.Collection;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.Element;
import org.dom4j.QName;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class RegisterSet implements PacketProcessor<IQ> {
	public static final String ELEMENT_NAME = "query";
	private static final Logger LOGGER = Logger.getLogger(RegisterSet.class);

	private final BlockingQueue<Packet> outQueue;
	private final ChannelManager channelManager;
	private IQ request;
	private IQ response;
	private final Configuration configuration;

	public RegisterSet(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
		this(Configuration.getInstance(), outQueue, channelManager);
	}
	
	public RegisterSet(Configuration configuration, BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
		this.configuration = configuration;
		this.outQueue = outQueue;
		this.channelManager = channelManager;
	}

	@Override
	public void process(IQ reqIQ) throws Exception {
		this.request = reqIQ;
		this.response = IQ.createResultIQ(reqIQ);
		
		LOGGER.debug("Processing register request from " + request.getFrom());

		String domain = reqIQ.getFrom().getDomain();
		if (!domain.equals(Configuration.getInstance().getServerDomain())) {
			notThisDomain(reqIQ);
			return;
		}
		
		if (userRegistered()) {
			LOGGER.debug("User " + request.getFrom().toBareJID() + " is already registered");
			outQueue.put(response);
			return;
		}

		LOGGER.debug("Registering new user " + request.getFrom());
		
		channelManager.createPersonalChannel(request.getFrom());
		outQueue.put(response);
		autosubscribeToChannels(request.getFrom());
	}

	private void notThisDomain(IQ reqIQ) throws InterruptedException {
		// Request is coming from different domain than the
		// component is using. We will not allow this because
		// "buddycloud federation" cannot work for that.
		IQ reply = IQ.createResultIQ(reqIQ);
		reply.setType(Type.error);
		reply.setChildElement(reqIQ.getChildElement().createCopy());
		PacketError pe = new PacketError(
				org.xmpp.packet.PacketError.Condition.not_allowed,
				org.xmpp.packet.PacketError.Type.cancel);
		reply.setError(pe);
		outQueue.put(reply);
	}
	
	private boolean userRegistered() throws Exception {
		return channelManager.nodeExists("/user/" + request.getFrom().toBareJID() + "/posts");
	}

	// TODO: We should really be returning an error as per spec shouldn't we?
	// It should be up to the client to ignore the error, not the server.
	@SuppressWarnings("unused")
	private void userAlreadyRegistered() throws InterruptedException {
		// User is already registered.
		IQ reply = IQ.createResultIQ(request);
		reply.setType(Type.error);
		reply.setChildElement(request.getChildElement().createCopy());
		PacketError pe = new PacketError(
				org.xmpp.packet.PacketError.Condition.conflict,
				org.xmpp.packet.PacketError.Type.cancel);
		reply.setError(pe);
		outQueue.put(reply);
	}

	private void autosubscribeToChannels(final JID from) {
		Collection<JID> channels = configuration.getAutosubscribeChannels();

		if (LOGGER.isDebugEnabled()) {
			LOGGER.debug("Auto-subscribing " + from + " to " + channels.size()
					+ " channel(s)");
		}

		for (JID channel : channels) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("Auto-subscribing " + from + " to " + channel);
			}

			IQ subscribe = new IQ();

			subscribe.setType(Type.set);

			Element el = subscribe.getElement();
			Element pubsubEl = el.addElement("pubsub",
					JabberPubsub.NAMESPACE_URI);
			Element subscribeEl = pubsubEl.addElement("subscribe");

			String channelNodeId = Conf.getPostChannelNodename(channel);

			subscribeEl.addAttribute("node", channelNodeId);
			subscribeEl.addAttribute("jid", from.toBareJID().toString());

			try {
				if (channelManager.isLocalJID(channel)) {
					subscribe.setFrom(from);
					subscribe.setTo(configuration.getServerChannelsDomain());
				} else {
					subscribe.setFrom(configuration.getServerChannelsDomain());
					subscribe.setTo(channel.getDomain());

					Element actorEl = pubsubEl.addElement(QName.get("actor",
							JabberPubsub.NS_BUDDYCLOUD));

					actorEl.setText(from.toBareJID());
				}

				outQueue.put(subscribe);

				// If auto-approve is set, and this is a local private channel
				// then set the user to subscribed
				if (configuration.getBooleanProperty(
						Configuration.CONFIGURATION_CHANNELS_AUTOSUBSCRIBE_AUTOAPPROVE,
						false)
						&& channelManager.isLocalJID(channel)
						&& AccessModels.authorize.toString().equals(
								channelManager.getNodeConfValue(channelNodeId,
										Conf.ACCESS_MODEL))) {
					channelManager
							.addUserSubscription(new NodeSubscriptionImpl(
									channelNodeId, from,
									Subscriptions.subscribed));

					channelManager.setUserAffiliation(channelNodeId, from,
							channelManager
									.getDefaultNodeAffiliation(channelNodeId));
				}
			} catch (InterruptedException e) {
				LOGGER.error("Could not auto-subscribe " + from + " to "
						+ channel, e);
			} catch (NodeStoreException e) {
				LOGGER.error("Could not auto-subscribe " + from + " to "
						+ channel, e);
			}
		}
	}
}