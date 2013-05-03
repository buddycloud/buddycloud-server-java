package org.buddycloud.channelserver.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;

import org.logicalcobwebs.proxool.ProxoolException;
import org.logicalcobwebs.proxool.configuration.PropertyConfigurator;
import org.buddycloud.channelserver.Configuration;

public class JDBCConnectionFactory {

	private String proxoolAlias = "proxool.channelserver";
	private Properties driverProperties;

	public JDBCConnectionFactory(Configuration conf)
			throws ClassNotFoundException, ProxoolException {

		Class.forName("org.logicalcobwebs.proxool.ProxoolDriver");
	}

	Connection getConnection() throws SQLException {
		return DriverManager.getConnection(proxoolAlias);
	}
}
