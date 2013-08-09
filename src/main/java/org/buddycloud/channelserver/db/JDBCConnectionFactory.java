package org.buddycloud.channelserver.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;

import org.logicalcobwebs.proxool.ProxoolException;

public class JDBCConnectionFactory {

	private Properties driverProperties;

	public JDBCConnectionFactory(Properties conf)
			throws ClassNotFoundException, ProxoolException {
		Class.forName("org.logicalcobwebs.proxool.ProxoolDriver");
		this.driverProperties = conf;
	}

	Connection getConnection() throws SQLException {
		return DriverManager.getConnection(
				driverProperties.getProperty("jdbc.proxool.driver-url"), 
				driverProperties.getProperty("jdbc.user"),
				driverProperties.getProperty("jdbc.password"));
	}
}
