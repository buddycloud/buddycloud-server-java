package org.buddycloud.channelserver.utils;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.Component;
import org.xmpp.component.ComponentException;

public class ComponentBase {

  protected int socket;
  protected String hostname;
  protected String domain;
  protected String password;
  protected Component engine;
  
  private static final Logger LOGGER = Logger.getLogger(ComponentBase.class);
  
  protected void initialize(final Configuration configuration, final String domain) {
    this.domain = domain;
    hostname = configuration.getXmppHost();
    socket = Integer.parseInt(configuration.getComponentPort());
    password = configuration.getProperty("xmpp.secretkey");
  }
  
  public boolean run() throws ComponentException {
    try {
      ExternalComponentManager manager = new ExternalComponentManager(hostname, socket);
      manager.setDefaultSecretKey(password);
      manager.addComponent(domain, engine);
    } catch (ComponentException e) {
      LOGGER.error(e.getMessage());
      return false;
    }
    return true;
  }
}
