package org.buddycloud.channelserver.packetprocessor.iq.namespace.mam;

import java.io.StringReader;
import java.util.Date;
import java.util.concurrent.BlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.IQ.Type;
import org.xmpp.packet.Message;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;
import org.xmpp.resultsetmanagement.ResultSet;

public class MessageArchiveManagement implements PacketProcessor<IQ> {

    public static final String NAMESPACE_MAM = "urn:xmpp:mam:tmp";
    public static final String NAMESPACE_FORWARDED = "urn:xmpp:forward:0";
    public static final String NAMESPACE_DELAY = "urn:xmpp:delay";

    public static final String ELEMENT_NAME = "query";
    private static final Logger logger = Logger.getLogger(MessageArchiveManagement.class);
    private final BlockingQueue<Packet> outQueue;
    private ChannelManager channelManager;

    private SAXReader xmlReader = new SAXReader();
    private Message wrapper;

    private Date startTimestamp;
    private Date endTimestamp = new Date();

    private IQ requestIq;
    private IQ reply;

    public MessageArchiveManagement(BlockingQueue<Packet> outQueue, ChannelManager channelManager) {
        this.outQueue = outQueue;
        this.channelManager = channelManager;
    }

    @Override
    public void process(IQ reqIQ) throws Exception {
        requestIq = reqIQ;
        reply = IQ.createResultIQ(requestIq);

		if (false == Configuration.getInstance().isLocalJID(requestIq.getFrom())) {
			this._sendNotHandledStanza();
			return;
		}

        if (false == isValidRequest()) {
            return;
        }

        generateMessageWrapper();
        sendSubscriptionUpdates();
        sendAffiliationUpdated();
        sendItemUpdates();
        outQueue.put(IQ.createResultIQ(this.requestIq));
    }

    private void generateMessageWrapper() {
        wrapper = new Message();
        wrapper.setFrom(requestIq.getTo());
        wrapper.setTo(requestIq.getFrom());
        Element result = wrapper.addChildElement("result", NAMESPACE_MAM);
        result.addAttribute("id", requestIq.getID());
        Element forwarded = wrapper.addChildElement("forwarded", NAMESPACE_FORWARDED);
        Element delay = forwarded.addElement("delay", NAMESPACE_DELAY);
        delay.addAttribute("stamp", Conf.formatDate(new Date()));
        Element message = forwarded.addElement("msg");
        message.addAttribute("type", Message.Type.headline.toString());
        message.addAttribute("to", requestIq.getFrom().toFullJID());
        message.addAttribute("from", requestIq.getTo().toString());
    }

    private boolean isValidRequest() throws InterruptedException {
        try {
            Element query = requestIq.getChildElement();
            startTimestamp = Conf.parseDate("1970-01-01T00:00:00Z");
            if (query.element("start") != null) {
                startTimestamp = Conf.parseDate(query.elementText("start"));
            }
            if (query.element("end") != null) {
                endTimestamp = Conf.parseDate(query.elementText("end"));
            }
            return true;
        } catch (IllegalArgumentException e) {
            logger.error(e);
            sendErrorPacket(PacketError.Type.modify, PacketError.Condition.bad_request);
            return false;
        }
    }

    private void sendItemUpdates() {
        try {
            CloseableIterator<NodeItem> items = channelManager.getNewNodeItemsForUser(requestIq.getFrom(), startTimestamp, endTimestamp);
            if (false == items.hasNext()) {
                return;
            }

            Message notification = wrapper.createCopy();
            Element forwarded = notification.getElement().element("forwarded");
            notification.getElement().addAttribute("remote-server-discover", "false");
            Element event = forwarded.addElement("event");
            event.addNamespace("", JabberPubsub.NS_PUBSUB_EVENT);
            Element itemsElement = event.addElement("items");
            Element i = itemsElement.addElement("item");

            NodeItem item;
            while (items.hasNext()) {
                item = items.next();
                itemsElement.addAttribute("node", item.getNodeId());
                i.addAttribute("id", item.getId());

                if (null != i.element("entry")) {
                    i.remove(i.element("entry"));
                }
                i.add(xmlReader.read(new StringReader(item.getPayload())).getRootElement());

                forwarded.element("delay").addAttribute("stamp", Conf.formatDate(item.getUpdated()));
                outQueue.put(notification.createCopy());
            }
        } catch (NodeStoreException e) {
            logger.error(e);
        } catch (InterruptedException e) {
            logger.error(e);
        } catch (DocumentException e) {
            logger.error(e);
        }
    }

    private void sendAffiliationUpdated() {
        try {
            ResultSet<NodeAffiliation> changes = channelManager.getAffiliationChanges(requestIq.getFrom(), startTimestamp, endTimestamp);
            if (0 == changes.size()) {
                return;
            }
            Message notification = wrapper.createCopy();
            Element forwarded = notification.getElement().element("forwarded");

            Element event = forwarded.addElement("event");
            Element affiliations = event.addElement("affiliations");
            Element affiliationElement = affiliations.addElement("affiliation");
            event.addNamespace("", JabberPubsub.NS_PUBSUB_EVENT);
            Affiliations affiliation;
            for (NodeAffiliation change : changes) {
                affiliation = change.getAffiliation();
                if ((true == isOwnerModerator(change.getNodeId())) && (true == Affiliations.outcast.equals(change.getAffiliation()))) {
                    affiliation = Affiliations.none;
                }
                affiliations.addAttribute("node", change.getNodeId());
                affiliationElement.addAttribute("jid", change.getUser().toBareJID());
                affiliationElement.addAttribute("affiliation", affiliation.toString());
                forwarded.element("delay").addAttribute("stamp", Conf.formatDate(change.getLastUpdated()));
                outQueue.put(notification.createCopy());
            }
        } catch (NodeStoreException e) {
            logger.error(e);
        } catch (InterruptedException e) {
            logger.error(e);
        }
    }

    private void sendSubscriptionUpdates() {
        try {
            ResultSet<NodeSubscription> changes = channelManager.getSubscriptionChanges(requestIq.getFrom(), endTimestamp, endTimestamp);
            if (0 == changes.size()) {
                return;
            }
            Message notification = wrapper.createCopy();
            Element forwarded = notification.getElement().element("forwarded");

            Element event = forwarded.addElement("event");
            event.addNamespace("", JabberPubsub.NS_PUBSUB_EVENT);
            Element subscription = event.addElement("subscription");

            for (NodeSubscription change : changes) {
                if ((false == isOwnerModerator(change.getNodeId())) && (true == Subscriptions.invited.equals(change.getSubscription()))) {
                    continue;
                }
                subscription.addAttribute("node", change.getNodeId());
                subscription.addAttribute("jid", change.getUser().toBareJID());
                subscription.addAttribute("subscription", change.getSubscription().toString());
                if ((null != change.getInvitedBy()) && Subscriptions.invited.equals(change.getSubscription())) {
                    subscription.addAttribute("invited-by", change.getInvitedBy().toBareJID());
                }
                forwarded.element("delay").addAttribute("stamp", Conf.formatDate(change.getLastUpdated()));
                outQueue.put(notification.createCopy());
            }
        } catch (NodeStoreException e) {
            logger.error(e);
        } catch (InterruptedException e) {
            logger.error(e);
        }
    }

    private void _sendNotHandledStanza() throws InterruptedException {
        sendErrorPacket(PacketError.Type.cancel, PacketError.Condition.service_unavailable);
    }

    private void sendErrorPacket(PacketError.Type type, Condition condition) throws InterruptedException {
        reply.setChildElement(requestIq.getChildElement().createCopy());
        reply.setType(Type.error);
        PacketError pe = new PacketError(condition, type);
        reply.setError(pe);
        outQueue.put(reply);
    }

    private boolean isOwnerModerator(String node) throws NodeStoreException {
        return channelManager.getNodeMembership(node, requestIq.getFrom()).getAffiliation().canAuthorize();
    }
}
