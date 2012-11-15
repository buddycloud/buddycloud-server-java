package org.buddycloud.channelserver;

import java.util.Properties;

import org.apache.log4j.Logger;
import org.jivesoftware.whack.ExternalComponentManager;
import org.xmpp.component.ComponentException;
import org.logicalcobwebs.proxool.ProxoolException;
import org.logicalcobwebs.proxool.configuration.PropertyConfigurator;

public class XmppComponent {

	private static final String DATABASE_CONFIGURATION_FILE = "db.properties";
	
	private static final Logger logger = Logger.getLogger(XmppComponent.class);
	private String hostname;
	private int socket;
	
	private String domainName;
	private String password;
	private Properties conf;
	
	public XmppComponent(Properties conf) {
	    setConf(conf);
		hostname = conf.getProperty("xmpp.host");
		socket = Integer.valueOf(conf.getProperty("xmpp.port"));
		domainName = conf.getProperty("xmpp.subdomain");
		password = conf.getProperty("xmpp.secretkey");

		try {
			PropertyConfigurator.configure(DATABASE_CONFIGURATION_FILE);
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
		manager.addComponent(this.domainName, new ChannelsEngine(conf));
	}
}