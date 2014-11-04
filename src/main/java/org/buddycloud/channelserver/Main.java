package org.buddycloud.channelserver;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.logicalcobwebs.proxool.ProxoolException;
import org.xmpp.component.ComponentException;

public class Main {

  private static final Logger LOGGER = Logger.getLogger(Main.class);
  public static final String MISSING_CHANNEL_COMPONENT_CONFIGURATION = "Property server.domain.channels is mandatory";
  private static Configuration configuration;
  private static long componentConnectionDelay;

  public static void main(String[] args) throws Exception {
    startComponents();
  }

  private static void startComponents() throws Exception {
    PropertyConfigurator.configure("log4j.properties");
    LOGGER.info("Starting Buddycloud channel mockup version...");

    configuration = Configuration.getInstance();

    componentConnectionDelay = Long.parseLong(
        configuration.getProperty(Configuration.COMPONENT_STARTUP_DELAY, "5000")
    );
        
    LOGGER.info("Connecting to '" + configuration.getXmppHost() + ":"
        + configuration.getComponentPort() + "' and trying to claim address '"
        + configuration.getProperty("server.domain") + "'.");

    int channelCounter = 0;
    while (false == startChannelComponent()) {
      Thread.sleep(componentConnectionDelay);
      LOGGER.info("Waiting for component connection (attempt " + channelCounter + ")");
      ++channelCounter;
      if (channelCounter > 5) {
        throw new Exception("Unable to connect channel component");
      }
    }
    int topicCounter = 0;
    while (false == startTopicComponent()) {
      Thread.sleep(componentConnectionDelay);
      LOGGER.info("Waiting for topic component connection (attempt " + topicCounter + ")");
      ++topicCounter;
      if (topicCounter > 5) {
        throw new Exception("Unable to connect topic component");
      }
    }
    hang();
  }

  private static boolean startTopicComponent() throws Exception {
    String topicDomain = configuration.getProperty(Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN);
    if (topicDomain == null) {
      return true;
    }
    TopicsComponent component = new TopicsComponent(configuration, topicDomain);
    return component.run();
  }

  private static boolean startChannelComponent() throws Exception {
    String channelDomain =
        configuration.getProperty(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN);
    if (channelDomain == null) {
      throw new IllegalArgumentException(MISSING_CHANNEL_COMPONENT_CONFIGURATION);
    }
    XmppComponent component = new XmppComponent(configuration, channelDomain);
    return component.run();
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
