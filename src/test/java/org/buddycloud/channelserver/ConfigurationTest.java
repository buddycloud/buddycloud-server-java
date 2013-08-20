package org.buddycloud.channelserver;

import static org.junit.Assert.*;

import java.util.Collection;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.JID;

public class ConfigurationTest {

	Configuration configuration;
	
	@Before
	public void setUp() throws Exception {
		configuration = Configuration.getInstance();
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

}
