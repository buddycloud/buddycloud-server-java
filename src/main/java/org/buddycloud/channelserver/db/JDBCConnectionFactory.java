package org.buddycloud.channelserver.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;

import org.apache.log4j.PropertyConfigurator;
import org.buddycloud.channelserver.Configuration;

public class JDBCConnectionFactory {
	
	private String proxoolAlias;
	
	private Properties driverProperties;
	
	public JDBCConnectionFactory(final Configuration conf) throws ClassNotFoundException {
		Class.forName("org.logicalcobwebs.proxool.ProxoolDriver");
		
		conf.setProperty("jdbc.proxool.alias", "channelserver");
		
		PropertyConfigurator.configure(conf);
	}
	
	Connection getConnection() throws SQLException {
		return DriverManager.getConnection("proxool.channelserver");
	}
}
