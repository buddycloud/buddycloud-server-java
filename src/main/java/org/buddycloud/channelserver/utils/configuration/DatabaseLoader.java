package org.buddycloud.channelserver.utils.configuration;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;

public class DatabaseLoader {

  private static final Logger LOGGER = Logger.getLogger(DatabaseLoader.class);
  private Configuration conf;
  private String connectionString;

  public DatabaseLoader(Configuration conf, String connectionString) {
    this.connectionString = connectionString;
    this.conf = conf;
  }

  public void load() throws SQLException {

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
      e.printStackTrace();
      e.getMessage();
      LOGGER.error("Could not get configuration from database");
      System.exit(1);
    } finally {
      if (null != connection) {
        connection.close();
      }
    }
  }

}
