package org.buddycloud.channelserver;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.logicalcobwebs.proxool.ProxoolException;
import org.xmpp.component.ComponentException;

public class Main {

  private static final Logger LOGGER = Logger.getLogger(Main.class);
  private static Configuration configuration;

  public static void main(String[] args) {
    try {
      startComponents();
    } catch (Exception e) {
      LOGGER.error("Failed during initialization.", e);
    }
  }

  private static void startComponents() throws Exception {
    PropertyConfigurator.configure("log4j.properties");
    LOGGER.info("Starting Buddycloud channel mockup version...");

    configuration = Configuration.getInstance();

    LOGGER.info("Connecting to '" + configuration.getXmppHost() + ":"
        + configuration.getComponentPort() + "' and trying to claim address '"
        + configuration.getProperty("server.domain") + "'.");

    while (false == startChannelComponent()) {
      Thread.sleep(5000);
      LOGGER.info("Waiting for component connection");
    }
    while (false == startTopicComponent()) {
      Thread.sleep(5000);
      LOGGER.info("Waiting for topic component connection");
    }
    hang();
  }

  private static boolean startTopicComponent() {
    String topicDomain = configuration.getProperty(Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN);
    if (topicDomain == null) {
      return true;
    }
    try {
      new TopicsComponent(configuration, topicDomain).run();
    } catch (ComponentException e) {
      return false;
    }
    return true;
  }

  private static boolean startChannelComponent() throws Exception {
    String channelDomain =
        configuration.getProperty(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN);
    if (channelDomain == null) {
      throw new IllegalArgumentException("Property server.domain.channels is mandatory.");
    }
    try {
      new XmppComponent(configuration, channelDomain).run();
    } catch (ComponentException e) {
      return false;
    } catch (ProxoolException e) {
      throw new NodeStoreException(e);
    }
    return true;
  }

  private static void hang() {
    while (true) {
      try {
        Thread.sleep(5000);
      } catch (InterruptedException e) {
        LOGGER.error(e);
      }
    }
  }
}
