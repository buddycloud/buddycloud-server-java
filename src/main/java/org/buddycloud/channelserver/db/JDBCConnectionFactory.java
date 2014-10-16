package org.buddycloud.channelserver.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import org.buddycloud.channelserver.Configuration;
import org.logicalcobwebs.proxool.ProxoolException;

public class JDBCConnectionFactory {

    private Configuration driverProperties;

    public JDBCConnectionFactory(Configuration conf) throws ClassNotFoundException, ProxoolException {
        Class.forName("org.logicalcobwebs.proxool.ProxoolDriver");
        this.driverProperties = conf;
    }

    Connection getConnection() throws SQLException {
        return DriverManager.getConnection(driverProperties.getDatabaseConnectionUrl());
    }
}
