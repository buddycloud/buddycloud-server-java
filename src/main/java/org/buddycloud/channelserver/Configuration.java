package org.buddycloud.channelserver;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Properties;

import org.xmpp.packet.JID;

public class Configuration extends Properties
{
	private static final long serialVersionUID = 1L;

	public static final String CONFIGURATION_SERVER_DOMAIN = "server.domain";
	public static final String CONFIGURATION_SERVER_CHANNELS_DOMAIN = "server.domain.channels";
	public static final String CONFIGURATION_SERVER_TOPICS_DOMAIN = "server.domain.topics";
	
	public static final String CONFIGURATION_ADMIN_USERS = "users.admin";
	
	private static final String CONFIGURATION_FILE = "configuration.properties";
	private static Configuration instance          = null;
	
	private ArrayList<JID> adminUsers = new ArrayList<JID>();
	
	private Properties conf;
	
    private Configuration()
    {
    	try {
	        conf = new Properties();
	        load(new FileInputStream(CONFIGURATION_FILE));
	        
	        if (conf.containsKey(CONFIGURATION_ADMIN_USERS))
	        	setupAdminUsers();
    	} catch (Exception e) {
            System.out.println(e.getMessage());
            System.exit(1);
    	}
    }
    
    private void setupAdminUsers() {
    	String[] jids = conf.getProperty(CONFIGURATION_ADMIN_USERS).split(";");
    	adminUsers.clear();
    	for (String jid : jids) {
    		adminUsers.add(new JID(jid));
    	}
	}
    
    public ArrayList<JID> getAdminUsers() {
    	return adminUsers;
    }

	public static Configuration getInstance() 
    {
    	if (null == instance) {
    		instance = new Configuration();
    	}
    	return instance;
    }
    
    public String getProperty(String key)
    {
    	return conf.getProperty(key);
    }
    
    public String getProperty(String key, String defaultValue)
    {
    	return conf.getProperty(key, defaultValue);
    }
    
    public void load(InputStream inputStream) throws IOException
    {
        conf.load(inputStream);
        if (conf.containsKey(CONFIGURATION_ADMIN_USERS))
        	setupAdminUsers();
    }
}