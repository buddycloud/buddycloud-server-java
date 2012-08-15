package org.buddycloud.channelserver;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.xmpp.component.ComponentException;
import org.buddycloud.channelserver.Configuration;

public class Main
{
    private static Logger LOGGER = Logger.getLogger(Main.class);
    
    public static void main(String[] args) {
        
        PropertyConfigurator.configure("log4j.properties");
        Logger.getLogger(Main.class).setLevel(Level.DEBUG);
        
        LOGGER.info("Starting Buddycloud channel mockup version...");

        try {
        	Configuration conf = Configuration.getInstance(); 

            LOGGER.info("Connecting to '" + conf.getProperty("xmpp.host") + ":" 
                + conf.getProperty("xmpp.port") 
                + "' and trying to claim address '" 
                + conf.getProperty("xmpp.subdomain") + "'.");

	        try {
	            XmppComponent xmppComponent = new XmppComponent(
	                conf.getProperty("xmpp.host"),
	                Integer.valueOf(conf.getProperty("xmpp.port")), 
	                conf.getProperty("xmpp.subdomain"), 
	                conf.getProperty("xmpp.secretkey")
	            );
	            xmppComponent.setConf(conf);
	            xmppComponent.run();
	        } catch (ComponentException e1) {
	            // TODO Auto-generated catch block
	            e1.printStackTrace();
	        }
	        run();
        } catch (IOException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
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