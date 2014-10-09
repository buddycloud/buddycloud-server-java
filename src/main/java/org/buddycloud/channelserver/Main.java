package org.buddycloud.channelserver;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

public class Main {

    private static final Logger LOG = Logger.getLogger(Main.class);

    public static void main(String[] args) {
        try {
            startComponents();
        } catch (Exception e) {
            LOG.error("Failed during initialization.", e);
        }
    }

    private static void startComponents() throws Exception {
        PropertyConfigurator.configure("log4j.properties");
        LOG.info("Starting Buddycloud channel mockup version...");

        Configuration configuration = Configuration.getInstance();

        LOG.info("Connecting to '" + configuration.getXmppHost() + ":" + configuration.getComponentPort() + "' and trying to claim address '"
                + configuration.getProperty("server.domain") + "'.");

        String channelDomain = configuration.getProperty(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN);
        String topicDomain = configuration.getProperty(Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN);

        if (channelDomain == null) {
            throw new IllegalArgumentException("Property server.domain.channels is mandatory.");
        }
        new XmppComponent(configuration, channelDomain).run();

        if (topicDomain != null) {
            new TopicsComponent(configuration, topicDomain).run();
        }
        hang();
    }

    private static void hang() {
        while (true) {
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                LOG.error(e);
            }
        }
    }
}
