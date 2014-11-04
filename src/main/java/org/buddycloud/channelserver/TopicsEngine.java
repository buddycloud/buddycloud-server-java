package org.buddycloud.channelserver;

import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.dom4j.Element;
import org.xmpp.component.Component;
import org.xmpp.component.ComponentException;
import org.xmpp.component.ComponentManager;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

public class TopicsEngine implements Component {

    private static final Logger LOGGER = Logger.getLogger(TopicsEngine.class);

    private JID jid = null;
    private ComponentManager manager = null;

    private Properties configuration;

    public TopicsEngine(Properties conf) {
        this.configuration = conf;
    }

    @Override
    public String getDescription() {
        return "buddycloud channel server (Java implementation) - topics component";
    }

    @Override
    public String getName() {
        return "buddycloud-topics-component";
    }

    @Override
    public void initialize(JID jid, ComponentManager manager) throws ComponentException {

        this.jid = jid;
        this.manager = manager;

        LOGGER.info("XMPP ComponentBase started. We are '" + jid.toString() + "' and ready to accept packages.");
    }

    @Override
    public void processPacket(Packet p) {
        try {
            if (p instanceof IQ) {
                if (p.getElement().element("query").getNamespace().getStringValue().equals(JabberPubsub.NS_DISCO_INFO)) {
                    // TODO(garethf) support info requests
                } else if (p.getElement().element("query").getNamespace().getStringValue().equals(JabberPubsub.NS_DISCO_ITEMS)) {
                    IQ response = IQ.createResultIQ((IQ) p);
                    Element query = response.getElement().addElement("query");
                    query.addNamespace("", JabberPubsub.NS_DISCO_ITEMS);
                    Element item = query.addElement("item");
                    item.addAttribute("name", "buddycloud-server");
                    item.addAttribute("jid", configuration.getProperty("server.domain.channels"));
                    this.sendPacket(response);
                    return;
                }
            }

        } catch (NullPointerException e) {
            // Catch and ignore
            e.printStackTrace();
            LOGGER.error(e);
        } catch (ComponentException e) {
            LOGGER.error(e);
        }
        LOGGER.info("Topic component does not handle these packets: '" + p.toXML() + "'.");
    }

    public void sendPacket(Packet p) throws ComponentException {
        manager.sendPacket(this, p);
    }

    @Override
    public void shutdown() {
        
    }

    @Override
    public void start() {
        /**
         * Notification message indicating that the component will start receiving incoming packets.
         * At this time the component may finish pending initialization issues that require
         * information obtained from the server.
         * 
         * It is likely that most of the component will leave this method empty.
         */
    }

    public JID getJID() {
        return this.jid;
    }
}
