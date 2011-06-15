package org.buddycloud.channels;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import org.xmpp.component.ComponentException;

public class Main {
	
	private static final String CONFIGURATION_FILE = "configuration.properties";
	
	public static void main(String[] args) {
		
		Properties conf = new Properties();
		try {
			conf.load(new FileInputStream(CONFIGURATION_FILE));
		} catch (IOException e) {
			System.out.println(e.getMessage());
			System.exit(1);
		}
		
		try {
	
			XmppComponent xmppComponent = new XmppComponent(conf.getProperty("xmpp.host"),
															Integer.valueOf(conf.getProperty("xmpp.port")), 
															conf.getProperty("xmpp.subdomain"), 
								  							conf.getProperty("xmpp.secretkey"));
			xmppComponent.run();
			
		} catch (ComponentException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		while (true) {
			try {
				Thread.sleep(5000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
	}
	
}
