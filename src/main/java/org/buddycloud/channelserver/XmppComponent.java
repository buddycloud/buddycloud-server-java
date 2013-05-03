package org.buddycloud.channelserver;

import java.util.Properties;

import org.apache.log4j.Logger;
import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.ComponentException;
import org.logicalcobwebs.proxool.ProxoolException;
import org.logicalcobwebs.proxool.configuration.PropertyConfigurator;

public class XmppComponent {

	private static final String DATABASE_CONFIGURATION_FILE = "configuration.properties";
	
	private static final Logger logger = Logger.getLogger(XmppComponent.class);
	private String hostname;
	private int socket;
	
	private String domain;
	private String password;
	private Properties configuration;
	private ChannelsEngine channelsEngine;
	
	public XmppComponent(Properties configuration, String domain) {
	    setConf(configuration);
		hostname = configuration.getProperty("xmpp.host");
		socket = Integer.valueOf(configuration.getProperty("xmpp.port"));
		this.domain = domain;
		password = configuration.getProperty("xmpp.secretkey");
		channelsEngine = new ChannelsEngine(configuration);

		try {
			PropertyConfigurator.configure(DATABASE_CONFIGURATION_FILE);
		} catch (ProxoolException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void setConf(Properties configuration) {
		this.configuration = configuration;
	}
	
	public void run() throws ComponentException {
		ExternalComponentManager manager = new ExternalComponentManager(
		        hostname, socket);
		manager.setDefaultSecretKey(password);
		manager.addComponent(domain, channelsEngine);
	}
}