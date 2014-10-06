package org.buddycloud.channelserver.channel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.xmpp.packet.JID;

public class ChannelManagerImplTest {

	private static final String TEST_DOMAIN = "domain.com";
	private static final String TEST_TOPICS_DOMAIN = "topics.domain.com";
	
	@Mock
	NodeStore nodeStore;
	
	String user1 = "/user/user@" + TEST_DOMAIN + "/posts";
	String user2 = "/user/user@server2.com/posts";
	String user3 = "/user/user@topics.server3.com/posts";
	String user4 = "/user/user@" + TEST_TOPICS_DOMAIN + "/status";
	String user5 = "/user/user@server1.com/meta";
	
	/**
	 * Class under test
	 */
	ChannelManagerImpl channelManager;
	
	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		channelManager = new ChannelManagerImpl(nodeStore);
		
		// This is used loads
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_DOMAIN, TEST_DOMAIN);
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN,
				TEST_TOPICS_DOMAIN);
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER);
	}

	@After
	public void tearDown() throws Exception {
		channelManager = null;
	}

	@Test
	@Ignore("Until we can so something about the millisecond matching")
	public void testCreatePersonalChannel() throws Exception {
		JID channelJID = new JID("testchannel@domain.com");
		
		channelManager.createPersonalChannel(channelJID);

		verify(nodeStore).createNode(channelJID, Conf.getPostChannelNodename(channelJID), Conf.getDefaultPostChannelConf(channelJID));
		verify(nodeStore).createNode(channelJID, Conf.getStatusChannelNodename(channelJID), Conf.getDefaultStatusChannelConf(channelJID));
		verify(nodeStore).createNode(channelJID, Conf.getGeoPreviousChannelNodename(channelJID), Conf.getDefaultGeoPreviousChannelConf(channelJID));
		verify(nodeStore).createNode(channelJID, Conf.getGeoCurrentChannelNodename(channelJID), Conf.getDefaultGeoCurrentChannelConf(channelJID));
		verify(nodeStore).createNode(channelJID, Conf.getGeoNextChannelNodename(channelJID), Conf.getDefaultGeoNextChannelConf(channelJID));
	}

	@Test(expected=IllegalArgumentException.class)
	public void testCreatePersonalChannelFailsForRemoteUser() throws Exception {
		JID channelJID = new JID("testchannel@otherdomain.com");
		
		channelManager.createPersonalChannel(channelJID);
	}

	@Test
	public void testCreatePersonalWorksForExternallyValidatedDomain() throws Exception {
		Configuration.getInstance().putProperty(
				Configuration.CONFIGURATION_LOCAL_DOMAIN_CHECKER,
				Boolean.TRUE.toString());
		JID channelJID = new JID("testchannel@otherdomain.com");
		channelManager.createPersonalChannel(channelJID);
	}
	
	@Test
	@Ignore("Until we can so something about the millisecond matching")
	public void testCreatePersonalChannelSomeNodesExist() throws Exception {
		JID channelJID = new JID("testchannel@domain.com");
		
		when(nodeStore.nodeExists(Conf.getPostChannelNodename(channelJID))).thenReturn(true);
		
		channelManager.createPersonalChannel(new JID("testchannel@domain.com"));

		verify(nodeStore, never()).createNode(channelJID, Conf.getPostChannelNodename(channelJID), Conf.getDefaultPostChannelConf(channelJID));
		verify(nodeStore).createNode(channelJID, Conf.getStatusChannelNodename(channelJID), Conf.getDefaultStatusChannelConf(channelJID));
		verify(nodeStore).createNode(channelJID, Conf.getGeoPreviousChannelNodename(channelJID), Conf.getDefaultGeoPreviousChannelConf(channelJID));
		verify(nodeStore).createNode(channelJID, Conf.getGeoCurrentChannelNodename(channelJID), Conf.getDefaultGeoCurrentChannelConf(channelJID));
		verify(nodeStore).createNode(channelJID, Conf.getGeoNextChannelNodename(channelJID), Conf.getDefaultGeoNextChannelConf(channelJID));
	}
	
	@Test
	public void testIsLocalNodeSuccess() throws Exception {
		assertTrue(Configuration.getInstance().isLocalNode("/user/test@domain.com/posts"));
	}
	
	@Test
	public void testIsLocalNodeFailure() throws Exception {
		assertFalse(Configuration.getInstance().isLocalNode("/user/test@otherdomain.com/posts"));		
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIsLocalNodeWithInvalidNodeThrowsException() throws Exception {
		Configuration.getInstance().isLocalNode("somerandomnodeid");		
	}

	@Test
	public void testIsLocalJidForLocaJid() throws Exception {
		assertTrue(Configuration.getInstance().isLocalJID(new JID("user@" + TEST_DOMAIN)));
	}
	
	@Test
	public void testIsLocalJidForNonLocaJid() throws Exception {
		assertFalse(Configuration.getInstance().isLocalJID(new JID("user@server1.com")));
	}
	
	@Test
	public void testDeleteRemoteDataDeletesRemoteData() throws Exception {
		
		List<String> remoteNodes = new ArrayList<String>();
		remoteNodes.add(user2);
		remoteNodes.add(user3);
		remoteNodes.add(user5);
		
		when(nodeStore.getRemoteNodesList()).thenReturn(remoteNodes);
		
		channelManager.deleteRemoteData();
		
		verify(nodeStore).getRemoteNodesList();
		verify(nodeStore, Mockito.times(3)).purgeNodeItems(Mockito.anyString());
	}
	
	@Test
	public void testGetNodeDefaultAffiliationForNodeWithConf() throws Exception {
		when(nodeStore.getNodeConfValue(user1, Conf.DEFAULT_AFFILIATION)).thenReturn("moderator");
		
		Affiliations affiliation = channelManager.getDefaultNodeAffiliation(user1);
		
		assertEquals("Incorrect default affiliation", Affiliations.moderator, affiliation);
	}
	
	
	@Test
	public void testGetNodeDefaultAffiliationForNodeWithoutConf() throws Exception {
		Affiliations affiliation = channelManager.getDefaultNodeAffiliation(user1);
		
		// If nothing is specified, the default affiliation is "member"
		assertEquals("Incorrect default affiliation", Affiliations.member, affiliation);
	}
}