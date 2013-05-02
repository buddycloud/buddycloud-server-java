package org.buddycloud.channelserver.packetprocessor.presence;

import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.utils.users.OnlineResourceManager;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.Presence;

/**
 * <presence to="channels.surevine.com"
 *           from="lloyd.watkin@surevine.com/resource">
 *     <status>buddycloud</status>
 *     <show>na</show>
 *     <priority>-1</priority>
 * </presence>
 */
public class PresenceProcessorTest extends IQTestHandler {

	private PresenceProcessor presenceProcessor;
	private OnlineResourceManager onlineUsers;
	private Properties configuration;

	@Before
	public void setUp() throws Exception {

		configuration = Mockito.mock(Properties.class);
		Mockito.when(
				configuration
						.getProperty(Configuration.CONFIGURATION_SERVER_DOMAIN))
				.thenReturn("server1.com");

		onlineUsers = Mockito.mock(OnlineResourceManager.class);

		presenceProcessor = new PresenceProcessor(configuration, onlineUsers);
	}

	@Test(expected = NullPointerException.class)
	public void testExceptionThrownOnInstantiationIfConfigurationNotSetProperly()
			throws Exception {
		Properties configuration = Mockito.mock(Properties.class);
		presenceProcessor = new PresenceProcessor(configuration, onlineUsers);
	}

	@Test
	public void testNoFromAddressResultsInNoActions() throws Exception {
		presenceProcessor.process(new Presence());

		Mockito.verify(onlineUsers, Mockito.times(0)).updateStatus(
				Mockito.any(JID.class), Mockito.anyString());
	}
	
	@Test
	public void testFromAddressForAnotherServerIsIgnored() throws Exception {
		
		Presence presence = new Presence();
		presence.setFrom(new JID("user@server2.com/webclient"));
		presenceProcessor.process(presence);

		Mockito.verify(onlineUsers, Mockito.times(0)).updateStatus(
				Mockito.any(JID.class), Mockito.anyString());
	}
	
	@Test
	public void testMissingStatusElementResultsInNoAction() throws Exception {
		Presence presence = new Presence();
		presence.setFrom(new JID("user@server1.com/webclient"));
		presence.getElement().addElement("status").elementText("buddycloud");
		
		presenceProcessor.process(presence);

		Mockito.verify(onlineUsers, Mockito.times(0)).updateStatus(
				Mockito.any(JID.class), Mockito.anyString());
	}
	
	@Test
	public void testAcceptablePresencePacketCausesActionsOnResourceManager() throws Exception {
		Presence presence = new Presence();
		presence.setFrom(new JID("user@server1.com/webclient"));
		presence.getElement().addElement("show").addText("chat");

		presenceProcessor.process(presence);

		Mockito.verify(onlineUsers, Mockito.times(1)).updateStatus(
				Mockito.any(JID.class), Mockito.eq("chat"));
	}
}