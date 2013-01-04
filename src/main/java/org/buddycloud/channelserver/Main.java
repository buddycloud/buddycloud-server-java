package org.buddycloud.channelserver;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.xmpp.component.ComponentException;

public class Main
{
    private static Logger LOGGER = Logger.getLogger(Main.class);
	
    public static void main(String[] args) {
        
        PropertyConfigurator.configure("log4j.properties");
        Logger.getLogger(Main.class).setLevel(Level.DEBUG);
        
        LOGGER.info("Starting Buddycloud channel mockup version...");

    	Configuration conf = Configuration.getInstance(); 

        LOGGER.info("Connecting to '" + conf.getProperty("xmpp.host") + ":" 
            + conf.getProperty("xmpp.port") 
            + "' and trying to claim address '" 
            + conf.getProperty("xmpp.subdomain") + "'.");

        try {
            XmppComponent xmppComponent = new XmppComponent(conf, conf.getProperty("server.domain.channels"));
            xmppComponent.run();
            
            TopicsComponent topicsComponent = new TopicsComponent(conf, conf.getProperty("server.domain.topics"));
            topicsComponent.run();
        } catch (ComponentException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        run();
    }

	private static void run()
	{
		while (true) {
		    try {
		        Thread.sleep(5000);
		    } catch (InterruptedException e) {
		        e.printStackTrace();
		    }
		}
	}
}