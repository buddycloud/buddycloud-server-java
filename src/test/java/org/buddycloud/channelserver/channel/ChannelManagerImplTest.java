package org.buddycloud.channelserver.channel;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import org.buddycloud.channelserver.db.NodeStore;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.xmpp.packet.JID;

public class ChannelManagerImplTest {

	@Mock
	NodeStore nodeStore;
	
	/**
	 * Class under test
	 */
	ChannelManagerImpl channelManager;
	
	@Before
	public void setUp() throws Exception {
		channelManager = new ChannelManagerImpl(nodeStore);
	}

	@After
	public void tearDown() throws Exception {
		channelManager = null;
	}

	@Test
	public void testCreateChannel() {
		Map<String,String> configuration = Collections.emptyMap();
		channelManager.createChannel(new JID("testchannel@localserver"), configuration);
		
		verify(nodeStore).createNode(owner, nodeRef, nodeConf)
	}

	@Test
	public void testUpdateChannelConfiguration() {
		fail("Not yet implemented");
	}

	@Test
	public void testChannelExists() {
		fail("Not yet implemented");
	}

	@Test
	public void testSetChannelSubscription() {
		fail("Not yet implemented");
	}

	@Test
	public void testSetChannelAffiliation() {
		fail("Not yet implemented");
	}

}
