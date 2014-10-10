package org.buddycloud.channelserver;

import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.ComponentException;

public class TopicsComponent {

    private String hostname;
    private int socket;

    private String domain;
    private String password;
    private TopicsEngine topicsEngine;

    public TopicsComponent(Configuration configuration, String domain) {
        hostname = configuration.getXmppHost();
        socket = Integer.parseInt(configuration.getComponentPort());
        this.domain = domain;
        password = configuration.getProperty("xmpp.secretkey");
        topicsEngine = new TopicsEngine(configuration);
    }

    public void run() throws ComponentException {
        ExternalComponentManager manager = new ExternalComponentManager(hostname, socket);
        manager.setDefaultSecretKey(password);
        manager.addComponent(domain, topicsEngine);
    }
}
