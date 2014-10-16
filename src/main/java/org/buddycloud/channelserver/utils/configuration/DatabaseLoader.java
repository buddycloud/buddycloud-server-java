package org.buddycloud.channelserver.utils.configuration;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;

public class DatabaseLoader implements Loader {

  private static final Logger LOGGER = Logger.getLogger(DatabaseLoader.class);
  private Configuration conf;
  private String connectionString;

  public DatabaseLoader(Configuration conf, String connectionString) {
    this.connectionString = connectionString;
    this.conf = conf;
  }

  public void load() throws ConfigurationException {

    Connection connection = null;
    LOGGER.info("Loading configuration from database");
    try {
      connection = DriverManager.getConnection(this.connectionString);
      PreparedStatement statement =
          connection.prepareStatement("SELECT \"key\", \"value\" FROM \"configuration\";");
      ResultSet rs = statement.executeQuery();
      while (rs.next()) {
        this.conf.setProperty(rs.getString(1), rs.getString(2));
      }
      this.conf.setProperty(Configuration.JDBC_CONNECTION_STRING, connectionString);
      this.conf.removeKey(Configuration.JDBC_USER);
      this.conf.removeKey(Configuration.JDBC_PASSWORD);
    } catch (SQLException e) {
      throw new ConfigurationException("Could not get configuration from database");
    } finally {
      if (null == connection) {
          return;
      }
      try {
        connection.close();
      } catch (SQLException e) {
        throw new ConfigurationException("Could not get configuration from database");
      }
    }
  }

}
