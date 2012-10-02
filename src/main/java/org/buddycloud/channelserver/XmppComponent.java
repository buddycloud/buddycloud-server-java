package org.buddycloud.channelserver;

import java.util.Properties;

import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.ComponentException;
import org.logicalcobwebs.proxool.ProxoolException;
import org.logicalcobwebs.proxool.configuration.PropertyConfigurator;

public class XmppComponent {

	private static final String JDBC_CONFIGURATION_FILE = "db.properties";
	private String hostname;
	private int socket;
	
	private String domainName;
	private String password;
	private Properties conf;
	
	public XmppComponent(String hostname, int socket, String domainName, String password) {
		this.hostname = hostname;
		this.socket = socket;
		this.domainName = domainName;
		this.password = password;
 
		try {
			PropertyConfigurator.configure(JDBC_CONFIGURATION_FILE);
		} catch (ProxoolException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public void setConf(Properties conf) {
		this.conf = conf;
	}
	
	public void run() throws ComponentException {
		ExternalComponentManager manager = new ExternalComponentManager(
		        this.hostname, this.socket);
		manager.setSecretKey(this.domainName, this.password);
		manager.addComponent(this.domainName, new ChannelsEngine(this.conf));
	}
}
