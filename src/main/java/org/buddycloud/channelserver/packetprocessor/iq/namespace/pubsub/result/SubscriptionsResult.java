package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.util.List;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class SubscriptionsResult extends PubSubElementProcessorAbstract {

    private IQ request;
    private boolean ownerRequest;
    private String lastNode = "";

    private static final Logger LOGGER = Logger.getLogger(SubscriptionsResult.class);

    public SubscriptionsResult(ChannelManager channelManager) {
        this.channelManager = channelManager;

        this.acceptedElementName = XMLConstants.SUBSCRIPTIONS_ELEM;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        this.request = reqIQ;

        if (-1 != request.getFrom().toString().indexOf("@")) {
            LOGGER.debug("Ignoring result packet, only interested in stanzas " + "from other buddycloud servers");
            return;
        }

        ownerRequest = ((null == node) || "".equals(node));

        addSubscriptions();
    }

    private void addSubscriptions() throws NodeStoreException {

        @SuppressWarnings("unchecked")
        List<Element> subscriptions =
                request.getElement().element(XMLConstants.PUBSUB_ELEM).element(XMLConstants.SUBSCRIPTIONS_ELEM).elements(XMLConstants.SUBSCRIPTION_ELEM);

        for (Element subscription : subscriptions) {
            addSubscription(subscription);
        }
    }

    private void addSubscription(Element subscription) throws NodeStoreException {

        if (ownerRequest) {
            node = subscription.attributeValue(XMLConstants.NODE_ATTR);
        }

        if ((!lastNode.equals(node)) && (!channelManager.nodeExists(node))) {
            channelManager.addRemoteNode(node);
        }

        JID jid = new JID(subscription.attributeValue(XMLConstants.JID_ATTR));

        JID listener = request.getFrom();
        if (Configuration.getInstance().isLocalJID(jid)) {
            listener = jid;
        }
        JID invitedBy = null;
        if (null != subscription.attributeValue(XMLConstants.INVITED_BY_ATTR)) {
            invitedBy = new JID(subscription.attributeValue(XMLConstants.INVITED_BY_ATTR));
        }

        NodeSubscription nodeSubscription =
                new NodeSubscriptionImpl(node, jid, listener, Subscriptions.createFromString(subscription.attributeValue(XMLConstants.SUBSCRIPTION_ELEM)),
                        invitedBy);
        channelManager.addUserSubscription(nodeSubscription);
        lastNode = node;
    }
}
