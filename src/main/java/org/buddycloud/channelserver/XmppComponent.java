package org.buddycloud.channelserver;

import org.jivesoftware.whack.ExternalComponentManager;
import org.logicalcobwebs.proxool.ProxoolException;
import org.logicalcobwebs.proxool.configuration.PropertyConfigurator;
import org.xmpp.component.ComponentException;

public class XmppComponent {

    private String hostname;
    private int socket;

    private String domain;
    private String password;
    private ChannelsEngine channelsEngine;

    public XmppComponent(Configuration configuration, String domain) throws ProxoolException {
        hostname = configuration.getXmppHost();
        socket = Integer.parseInt(configuration.getComponentPort());
        this.domain = domain;
        password = configuration.getProperty("xmpp.secretkey");
        channelsEngine = new ChannelsEngine(configuration);
        PropertyConfigurator.configure(configuration);
    }

    public void run() throws ComponentException {
        ExternalComponentManager manager = new ExternalComponentManager(hostname, socket);
        manager.setDefaultSecretKey(password);
        manager.addComponent(domain, channelsEngine);
    }
}
