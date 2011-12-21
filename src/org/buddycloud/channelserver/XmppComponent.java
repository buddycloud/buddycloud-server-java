package org.buddycloud.channelserver;

import java.util.Properties;

import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.ComponentException;

public class XmppComponent {

	private ExternalComponentManager manager;
	
	private String hostname;
	private int socket;
	
	private String domainName;
	private String password;
	
	private ChannelsEngine engine;
	
	private Properties conf;
	
	public XmppComponent(String hostname, int socket, String domainName, String password) {
		this.hostname = hostname;
		this.socket = socket;
		this.domainName = domainName;
		this.password = password;
	}
	
	public void setConf(Properties conf) {
		this.conf = conf;
	}
	
	public void run() throws ComponentException {
		
		this.manager = new ExternalComponentManager(this.hostname, this.socket);

		this.manager.setSecretKey(this.domainName, this.password);

		this.engine = new ChannelsEngine(this.conf);
		
		this.manager.addComponent(this.domainName, this.engine);
		
	}
	
}
