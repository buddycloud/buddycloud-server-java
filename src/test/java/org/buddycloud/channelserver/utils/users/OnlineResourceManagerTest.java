package org.buddycloud.channelserver.utils.users;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.JID;

public class OnlineResourceManagerTest extends IQTestHandler {

	private OnlineResourceManager onlineUser;
	private Properties configuration;
	private JID localUserLaptop    = new JID("user@server1.com/laptop");
	private JID localUserDesktop    = new JID("user@server1.com/desktop");
	private JID localUserNoResource = new JID("user@server1.com");
	private JID remoteBuddycloudServer = new JID("channels.buddycloud.org");
	
	private JID remoteUser = new JID("user@server2.com/remote");

	@Before
	public void setUp() throws Exception {
		configuration = Mockito.mock(Properties.class);
		Mockito.when(
				configuration
						.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN))
				.thenReturn("server1.com");

		onlineUser = new OnlineResourceManager(configuration);
	}

	@Test(expected=NullPointerException.class)
	public void testNotSettingCorrectConfigurationKeysThrowsException() {
		Properties configuration = Mockito.mock(Properties.class);
		Mockito.when(
				configuration
						.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN))
				.thenReturn(null);
		onlineUser = new OnlineResourceManager(configuration);
	}
	
	@Test
	public void testRequestingOnlineResourcesForOfflineUserReturnsNone() throws Exception {
		ArrayList<JID> users = onlineUser.getResources(localUserNoResource);
		assertEquals(0, users.size());
	}
	
	@Test
	public void testCanSetAUserAsOnlineAndSeeInOnlineResourcesList() throws Exception {
		
		onlineUser.updateStatus(localUserDesktop, "online");
		ArrayList<JID> users = onlineUser.getResources(localUserNoResource);
		assertEquals(1, users.size());
	}
	
	@Test
	public void testAttemptingToUpdateStatusOfUserNotFromDomainIsIgnored() throws Exception {
		
		onlineUser.updateStatus(remoteUser, "online");
		ArrayList<JID> users = onlineUser.getResources(new JID(remoteUser.toBareJID()));
		assertEquals(0, users.size());
	}
	
	@Test
	public void testCanGoOnlineOnTwoResourcesAndRetrieveBoth() throws Exception {
		onlineUser.updateStatus(localUserDesktop, "online");
		onlineUser.updateStatus(localUserLaptop, "online");
		ArrayList<JID> users = onlineUser.getResources(localUserNoResource);
		assertEquals(2, users.size());
	}
	
	@Test
	public void testCanGoOnlineOnTwoResourcesTakeOneOfflineAndRetrieveOneBack() throws Exception {
		onlineUser.updateStatus(localUserDesktop, "online");
		onlineUser.updateStatus(localUserLaptop, "online");
		onlineUser.updateStatus(localUserDesktop, onlineUser.UNAVAILABLE);
		ArrayList<JID> users = onlineUser.getResources(localUserNoResource);
		assertEquals(1, users.size());
		assertEquals(localUserLaptop, users.get(0));
	}
	
	@Test
	public void testCanConstructAJidIndependentlyAndItWillStillMatch() throws Exception {
		onlineUser.updateStatus(localUserDesktop, "online");
		onlineUser.updateStatus(localUserLaptop, "online");
		onlineUser.updateStatus(new JID(localUserDesktop.toFullJID()), onlineUser.UNAVAILABLE);
		ArrayList<JID> users = onlineUser.getResources(localUserNoResource);
		assertEquals(1, users.size());
		assertEquals(new JID(localUserLaptop.toFullJID()), users.get(0));
	}
	
	@Test
	public void testPassingAFullJidReturnsToGetResourcesOnlyReturnsThatJid() throws Exception {
		ArrayList<JID> users = onlineUser.getResources(localUserDesktop);
		assertEquals(1, users.size());
		assertEquals(localUserDesktop, users.get(0));
	}
	
	@Test
	public void testPassingRemoteBuddycloudServerReturnsOnlyThatJid() throws Exception {
		ArrayList<JID> users = onlineUser.getResources(remoteBuddycloudServer);
		assertEquals(1, users.size());
		assertEquals(remoteBuddycloudServer, users.get(0));
	}
}