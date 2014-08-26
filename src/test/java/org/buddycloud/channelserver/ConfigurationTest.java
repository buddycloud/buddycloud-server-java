package org.buddycloud.channelserver;

import static org.junit.Assert.*;

import java.io.FileInputStream;
import java.util.Collection;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.JID;

import org.apache.commons.io.IOUtils;

public class ConfigurationTest {

	private static final String CONFIGURATION_PROPERTIES = "src/test/resources/configuration.properties";

	Configuration configuration;

	@Before
	public void setUp() throws Exception {
		configuration = Configuration.getInstance();
		configuration.load(new FileInputStream(CONFIGURATION_PROPERTIES));
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testGetAutoSubscribeChannels() {
		Collection<JID> channels = configuration.getAutosubscribeChannels();
		
		assertEquals("Incorrect number of channels", 2,  channels.size());
		
		assertTrue("channel1@server1 not found", channels.contains(new JID("channel1@server1")));
		assertTrue("channel2@server1 not found", channels.contains(new JID("channel2@server1")));
	}
    
    @Test
    public void handlesNoEntryForAutosubscribeChannels() throws Exception {

        configuration.clear();
        configuration.load(IOUtils.toInputStream(
            Configuration.CONFIGURATION_SERVER_DOMAIN + "=domain.com\n"
        ));
        assertEquals(0, configuration.getAutosubscribeChannels().size());
    }
                     
    @Test
    public void handlesIllegalAutosubscribeChannels() throws Exception {
        String configurationString = "" +
            Configuration.CONFIGURATION_CHANNELS_AUTOSUBSCRIBE + "=not-a-jid$$\n";
        configuration.load(IOUtils.toInputStream(configurationString));
        assertEquals(0, configuration.getAutosubscribeChannels().size());  
    }
    
    @Test
    public void handlesNullAutosubscribeChannelsValue() throws Exception {
        String configurationString = "" +
            Configuration.CONFIGURATION_CHANNELS_AUTOSUBSCRIBE + "=\n";
        configuration.load(IOUtils.toInputStream(configurationString));
        assertEquals(0, configuration.getAutosubscribeChannels().size());  
    }

}
