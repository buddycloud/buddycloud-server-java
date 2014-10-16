package org.buddycloud.channelserver;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.LocalDomainChecker;
import org.buddycloud.channelserver.utils.configuration.DatabaseLoader;
import org.xmpp.packet.JID;

public class Configuration extends Properties {

  private static final Logger LOGGER = Logger.getLogger(Configuration.class);

  private static final long serialVersionUID = 1L;

  private static final String ARRAY_PROPERTY_SEPARATOR = ";";
  private static final String INVALID_NODE = "Illegal node format";

  public static final String CONFIGURATION_SERVER_DOMAIN = "server.domain";
  public static final String CONFIGURATION_SERVER_CHANNELS_DOMAIN = "server.domain.channels";
  public static final String CONFIGURATION_SERVER_TOPICS_DOMAIN = "server.domain.topics";
  public static final String CONFIGURATION_LOCAL_DOMAIN_CHECKER = "server.domain.checker";

  public static final String CONFIGURATION_ADMIN_USERS = "users.admin";

  public static final String CONFIGURATION_CHANNELS_AUTOSUBSCRIBE = "channels.autosubscribe";
  public static final String CONFIGURATION_CHANNELS_AUTOSUBSCRIBE_AUTOAPPROVE =
      "channels.autosubscribe.autoapprove";
  public static final String CONFIGURATION_CHANNELS_DEFAULT_AFFILIATION =
      "channel.configuration.default.affiliation";
  public static final String CONFIGURATION_CHANNELS_DEFAULT_ACCESSMODEL =
      "channel.configuration.default.accessmodel";
  public static final String CONFIGURATION_CHANNELS_DEFAULT_DESCRIPTION =
      "channel.configuration.default.description";
  public static final String CONFIGURATION_CHANNELS_DEFAULT_TITLE =
      "channel.configuration.default.title";

  public static final String DISCOVERY_USE_DNS = "discovery.dns.enabled";

  public static final String PERSIST_PRESENCE_DATA = "users.presence.persist";

  public static final String NOTIFICATIONS_SENDTO = "notifications.sendTo";
  public static final String NOTIFICATIONS_CONNECTED = "notifications.connected";

  private static final String CONFIGURATION_FILE = "configuration.properties";

  public static final String PURGE_REMOTE_ON_START = "sync.purge-on-start";

  public static final String XMPP_PORT = "xmpp.port";

  public static final String XMPP_HOST = "xmpp.host";

  public static final String DATABASE_ENV = "DATABASE";

  public static final String JDBC_CONNECTION_STRING = "jdbc.proxool.driver-url";
  public static final String JDBC_PASSWORD = "jdbc.password";
  public static final String JDBC_USER = "jdbc.user";

  private static Configuration instance = null;

  private Collection<JID> adminUsers = new ArrayList<JID>();
  private Collection<JID> autosubscribeChannels = new ArrayList<JID>();

  private Properties conf;

  private Configuration() {
    try {
      conf = new Properties();
      String databaseConnectionString = System.getenv(DATABASE_ENV);

      if (null == databaseConnectionString) {
        loadConfigurationFromFile();
      } else {
        DatabaseLoader loader = new DatabaseLoader(this, databaseConnectionString);
        loader.load();
      }

    } catch (Exception e) {
      LOGGER.error("Could not load configuration");
      System.exit(1);
    }
    setupCollections();
  }

  private void loadConfigurationFromFile() throws IOException {
    InputStream confFile = this.getClass().getClassLoader().getResourceAsStream(CONFIGURATION_FILE);
    if (confFile != null) {
      load(confFile);
      LOGGER.info("Loaded " + CONFIGURATION_FILE + " from classpath.");
    } else {
      File f = new File(CONFIGURATION_FILE);
      load(new FileInputStream(f));
      LOGGER.info("Loaded " + CONFIGURATION_FILE + " from working directory.");
    }
  }

  private void setupCollections() {
    adminUsers = getJIDArrayProperty(CONFIGURATION_ADMIN_USERS);
    autosubscribeChannels = getJIDArrayProperty(CONFIGURATION_CHANNELS_AUTOSUBSCRIBE);
  }

  public Collection<JID> getAdminUsers() {
    return adminUsers;
  }

  public Collection<JID> getAutosubscribeChannels() {
    return autosubscribeChannels;
  }

  public static Configuration getInstance() {
    if (null == instance) {
      instance = new Configuration();
    }
    return instance;
  }

  public static void reset() {
    instance = null;
  }

  public String getProperty(String key) {
    return conf.getProperty(key);
  }

  @Override
  public synchronized Object remove(Object key) {
    return conf.remove(key);
  }

  public void clear() {
    conf.clear();
  }

  public String getProperty(String key, String defaultValue) {
    return conf.getProperty(key, defaultValue);
  }

  public void putProperty(String key, String value) {
    conf.put(key, value);
  }

  public void load(InputStream inputStream) throws IOException {
    conf.load(inputStream);
  }

  private Collection<String> getStringArrayProperty(String key) {
    String prop = getProperty(key);

    if (null == prop) {
      return Collections.emptyList();
    }

    return Arrays.asList(prop.split(ARRAY_PROPERTY_SEPARATOR));
  }

  private Collection<JID> getJIDArrayProperty(String key) {
    Collection<String> props = getStringArrayProperty(key);

    Collection<JID> jids = new ArrayList<JID>(props.size());

    for (String prop : props) {
      try {
        jids.add(new JID(prop));
      } catch (IllegalArgumentException e) {
        LOGGER.error(e);
      }
    }

    return jids;
  }

  public ArrayList<JID> getNotificationsList(String event) {
    ArrayList<JID> notify = new ArrayList<JID>();
    if (!getBooleanProperty(event, false)) {
      return notify;
    }
    String[] users = getProperty(NOTIFICATIONS_SENDTO).split(";");
    JID userJid;
    for (String user : users) {
      try {
        userJid = new JID(user);
        notify.add(userJid);
      } catch (IllegalArgumentException e) {
        LOGGER.error(e);
      }
    }
    return notify;
  }

  public String getServerDomain() {
    return getProperty(CONFIGURATION_SERVER_DOMAIN);
  }

  public String getServerChannelsDomain() {
    return getProperty(CONFIGURATION_SERVER_CHANNELS_DOMAIN);
  }

  public String getServerTopicsDomain() {
    return getProperty(CONFIGURATION_SERVER_TOPICS_DOMAIN);
  }

  public boolean getBooleanProperty(final String key, final boolean defaultValue) {
    String value = getProperty(key);

    if (value != null) {
      if (value.equalsIgnoreCase("true")) {
        return true;
      }
      if (value.equalsIgnoreCase("false")) {
        return false;
      }
      LOGGER.warn("Invalid boolean property value for " + key + ": " + value);
    }

    return defaultValue;
  }

  public String getComponentPort() {
    return this.getProperty(XMPP_PORT, "5347");
  }

  public String getXmppHost() {
    return this.getProperty(XMPP_HOST, "127.0.0.1");
  }

  public boolean isLocalDomain(String domain) {
    return LocalDomainChecker.isLocal(domain, this);
  }

  public boolean isLocalNode(String nodeId) {
    if (false == nodeId.matches("/user/.+@.+/.+")) {
      LOGGER.debug("Node " + nodeId + " has an invalid format");
      throw new IllegalArgumentException(INVALID_NODE);
    }
    String domain = new JID(nodeId.split("/")[2]).getDomain();
    return isLocalDomain(domain);
  }

  public boolean isLocalJID(JID jid) {
    String domain = jid.getDomain();
    return isLocalDomain(domain);
  }

  public String getDatabaseConnectionUrl() {
    String url = this.getProperty(JDBC_CONNECTION_STRING);
    if (this.containsKey(JDBC_USER)) {
      url +=
          "?user=" + this.getProperty(JDBC_USER) + "&password="
              + this.getProperty(JDBC_PASSWORD);
    }
    return url;
  }

  public boolean containsKey(Object value) {
    return conf.containsKey(value);
  }
  
  public Object setProperty(String key, String value) {
    return conf.setProperty(key, value);
  }
  
  public void removeKey(String key) {
    conf.remove(key);
  }
}
