package org.buddycloud.channelserver;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class Configuration extends Properties
{
    private static final String CONFIGURATION_FILE = "configuration.properties";
	private static Configuration instance          = null;
	
	private Properties conf;
	
    private Configuration() throws FileNotFoundException, IOException
    {
        conf = new Properties();
        conf.load(new FileInputStream(CONFIGURATION_FILE));
    }
    
    public static Configuration getInstance() 
        throws FileNotFoundException, IOException
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
    }
}