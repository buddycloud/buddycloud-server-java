package org.buddycloud.channelserver.utils.configuration;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;

public class FileLoader implements Loader {

  private static final Logger LOGGER = Logger.getLogger(FileLoader.class);
  
  private static final String CONFIGURATION_FILE = "configuration.properties";
  
  private Configuration conf;

  public FileLoader(Configuration conf) {
    this.conf = conf;
  }

  public void load() throws ConfigurationException {
    InputStream confFile = this.getClass().getClassLoader().getResourceAsStream(CONFIGURATION_FILE);
    try {
      if (confFile != null) {
        readFile(confFile);
        LOGGER.info("Loaded " + CONFIGURATION_FILE + " from classpath."); 
      } else {
        File f = new File(CONFIGURATION_FILE);
        readFile(new FileInputStream(f));
        LOGGER.info("Loaded " + CONFIGURATION_FILE + " from working directory.");
      }
    } catch (IOException e) {
      throw new ConfigurationException("Could not load configuraton from file " + CONFIGURATION_FILE);
    }
  }

  private void readFile(InputStream inputStream) throws IOException {
    this.conf.load(inputStream);
  }

}
