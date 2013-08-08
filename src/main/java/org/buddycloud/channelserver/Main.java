package org.buddycloud.channelserver;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

public class Main {
    
	private static Logger LOGGER = Logger.getLogger(Main.class);
	
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

    	Configuration conf = Configuration.getInstance(); 

        LOGGER.info("Connecting to '" + conf.getProperty("xmpp.host") + ":" 
            + conf.getProperty("xmpp.port") 
            + "' and trying to claim address '" 
            + conf.getProperty("xmpp.subdomain") + "'.");

        String channelDomain = conf.getProperty("server.domain.channels");
        String topicDomain = conf.getProperty("server.domain.topics");
        
        if (channelDomain == null) {
        	throw new IllegalArgumentException("Property server.domain.channels is mandatory.");
        }
        new XmppComponent(conf, channelDomain).run();
        
        if (topicDomain != null) {
            new TopicsComponent(conf, topicDomain).run();
        }	
        hang();
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
