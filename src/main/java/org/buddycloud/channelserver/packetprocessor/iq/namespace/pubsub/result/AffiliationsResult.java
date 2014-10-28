package org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.result;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.PubSubElementProcessorAbstract;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.utils.XMLConstants;
import org.dom4j.Element;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

public class AffiliationsResult extends PubSubElementProcessorAbstract {

    private IQ request;
    private boolean ownerRequest;
    private String lastNode = "";

    private static final Logger LOGGER = Logger.getLogger(AffiliationsResult.class);

    public AffiliationsResult(ChannelManager channelManager) {
        this.channelManager = channelManager;

        acceptedElementName = XMLConstants.AFFILIATIONS_ELEM;
    }

    @Override
    public void process(Element elm, JID actorJID, IQ reqIQ, Element rsm) throws Exception {
        this.request = reqIQ;

        if (-1 != request.getFrom().toString().indexOf("@")) {
            LOGGER.debug("Ignoring result packet, only interested in stanzas " + "from other buddycloud servers");
            return;
        }

        ownerRequest = ((null == node) || "".equals(node));

        @SuppressWarnings("unchecked")
        List<Element> affiliations =
                reverseList(request.getElement().element(XMLConstants.PUBSUB_ELEM).element(XMLConstants.AFFILIATIONS_ELEM)
                        .elements(XMLConstants.AFFILIATION_ELEM));

        for (Element affiliation : affiliations) {
            addAffiliation(affiliation);
        }
    }

    private List<Element> reverseList(List<Element> originalList) {
        List<Element> invertedList = new ArrayList<Element>();
        for (int i = originalList.size() - 1; i >= 0; i--) {
            invertedList.add(originalList.get(i));
        }
        return invertedList;
    }

    private void addAffiliation(Element affiliation) throws NodeStoreException {

        if (ownerRequest) {
            node = affiliation.attributeValue(XMLConstants.NODE_ATTR);
        }

        if ((!lastNode.equals(node)) && (!channelManager.nodeExists(node))) {
            channelManager.addRemoteNode(node);
        }

        JID jid = new JID(affiliation.attributeValue(XMLConstants.JID_ATTR));
        channelManager.setUserAffiliation(node, jid, Affiliations.createFromString(affiliation.attributeValue(XMLConstants.AFFILIATION_ELEM)));
        lastNode = node;
    }
}
