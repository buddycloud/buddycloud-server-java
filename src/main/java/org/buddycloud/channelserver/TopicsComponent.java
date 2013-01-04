package org.buddycloud.channelserver;

import java.util.Properties;

import org.apache.log4j.Logger;
import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.ComponentException;
import org.logicalcobwebs.proxool.ProxoolException;
import org.logicalcobwebs.proxool.configuration.PropertyConfigurator;

public class TopicsComponent {

	private static final Logger logger = Logger.getLogger(TopicsComponent.class);
	private String hostname;
	private int socket;
	
	private String domain;
	private String password;
	private Properties configuration;
	private TopicsEngine topicsEngine;
	
	public TopicsComponent(Properties configuration, String domain) {
		if (null == domain) {
			return;
		}
	    setConf(configuration);
		hostname = configuration.getProperty("xmpp.host");
		socket = Integer.valueOf(configuration.getProperty("xmpp.port"));
		this.domain = domain;
		password = configuration.getProperty("xmpp.secretkey");
		topicsEngine = new TopicsEngine(configuration);

	}
	
	public void setConf(Properties configuration) {
		this.configuration = configuration;
	}
	
	public void run() throws ComponentException {
		ExternalComponentManager manager = new ExternalComponentManager(
		        hostname, socket);
		manager.setDefaultSecretKey(password);
		manager.addComponent(domain, topicsEngine);
	}
}