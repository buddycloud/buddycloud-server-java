package org.buddycloud.channelserver;

import static org.junit.Assert.fail;

import java.io.FileInputStream;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

public class MainTest {

    private static final String CONFIGURATION_PROPERTIES = "src/test/resources/configuration.properties";

    Configuration configuration;

    @Before
    public void setUp() throws Exception {
        configuration = Configuration.getInstance();
        configuration.load(new FileInputStream(CONFIGURATION_PROPERTIES));
    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void throwsExceptionIfNoChannelComponentConfigurationSet() throws Exception {

      configuration.removeKey(Configuration.CONFIGURATION_SERVER_CHANNELS_DOMAIN);
      try {
          Main.main(null);
          fail();
      } catch (IllegalArgumentException e) {
          Assert.assertEquals(Main.MISSING_CHANNEL_COMPONENT_CONFIGURATION, e.getMessage());
      }
    }
    
    @Test(expected=Exception.class)
    public void attemptsToReconnectIfInitialConnectionFails() throws Exception {
      configuration.setProperty(Configuration.COMPONENT_STARTUP_DELAY, "1");
      XmppComponent xmpp = Mockito.mock(XmppComponent.class);
      Mockito.when(xmpp.run()).thenReturn(false);
      Main.main(null);
    }
    
    @Test(expected=Exception.class)
    public void attemptsToReconnectIfTopicComponentInitialConnectionFails() throws Exception {
      configuration.setProperty(Configuration.COMPONENT_STARTUP_DELAY, "1");
      XmppComponent channelsComponent = Mockito.mock(XmppComponent.class);
      Mockito.when(channelsComponent.run()).thenReturn(true);
      TopicsComponent topicsComponent = Mockito.mock(TopicsComponent.class);
      Mockito.when(topicsComponent.run()).thenReturn(false);
      Main.main(null);
    }
}
