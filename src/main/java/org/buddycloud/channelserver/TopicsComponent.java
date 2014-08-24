package org.buddycloud.channelserver;

import java.util.Properties;

import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.ComponentException;

public class TopicsComponent {

	private String hostname;
	private int socket;
	
	private String domain;
	private String password;
	private TopicsEngine topicsEngine;
	
	public TopicsComponent(Properties configuration, String domain) {
		hostname = configuration.getProperty("xmpp.host");
		socket = Integer.valueOf(configuration.getComponentPort());
		this.domain = domain;
		password = configuration.getProperty("xmpp.secretkey");
		topicsEngine = new TopicsEngine(configuration);
	}
	
	public void run() throws ComponentException {
		ExternalComponentManager manager = new ExternalComponentManager(
		        hostname, socket);
		manager.setDefaultSecretKey(password);
		manager.addComponent(domain, topicsEngine);
	}
}