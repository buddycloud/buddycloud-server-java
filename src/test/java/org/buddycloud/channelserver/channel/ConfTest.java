package org.buddycloud.channelserver.channel;

import java.util.HashMap;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.junit.Test;
import static org.junit.Assert.*;
import org.xmpp.packet.JID;

/**
 * Simples regression test to assure refactoring changes will not break
 * something
 */
public class ConfTest {

	private JID testChannelJID = new JID("testchannel@domain.com");
	private JID testOwnerJID = new JID("testuser@domain.com");

	// @Test
	public void testGetPostChannelNodename() {
		fail("The test case is a prototype.");
	}

	// @Test
	public void testParseDate() {
		fail("The test case is a prototype.");
	}

	// @Test
	public void testFormatDate() {
		fail("The test case is a prototype.");
	}

	@Test
	public void testGetDefaultChannelConf() {
		HashMap<String, String> result = Conf.getDefaultChannelConf(
				testChannelJID, testOwnerJID);
		assertEquals(10, result.size());
		assertEquals(this.testChannelJID.toBareJID() + "'s title",
				result.get(Conf.TITLE));
		assertEquals(this.testChannelJID.toBareJID() + "'s description",
				result.get(Conf.DESCRIPTION));
		assertEquals("http://www.w3.org/2005/Atom", result.get(Conf.TYPE));
		assertEquals("publishers", result.get(Conf.PUBLISH_MODEL));
		assertEquals(AccessModels.open.toString(),
				result.get(Conf.ACCESS_MODEL));
		// ToDo: Find a way to test the Date: assertEquals(Conf.formatDate(new
		// Date()),result.get(Conf.CREATION_DATE));
		assertEquals(testOwnerJID.toBareJID(), result.get(Conf.OWNER));
		assertEquals(Affiliations.member.toString(),
				result.get(Conf.DEFAULT_AFFILIATION));
		assertEquals("1", result.get(Conf.NUM_SUBSCRIBERS));
		assertEquals("1", result.get(Conf.NOTIFY_CONFIG));
	}

	@Test
	public void testGetDefaultPostChannelConf() {
		HashMap<String, String> result = Conf
				.getDefaultPostChannelConf(testChannelJID);
		assertEquals(11, result.size());
		assertEquals(this.testChannelJID.toBareJID()
				+ "'s very own buddycloud channel!", result.get(Conf.TITLE));
		assertEquals(
				"This channel belongs to " + this.testChannelJID.toBareJID()
						+ ". To nobody else!", result.get(Conf.DESCRIPTION));
		assertEquals("http://www.w3.org/2005/Atom", result.get(Conf.TYPE));
		assertEquals("publishers", result.get(Conf.PUBLISH_MODEL));
		assertEquals(AccessModels.open.toString(),
				result.get(Conf.ACCESS_MODEL));
		// ToDo: Find a way to test the Date: assertEquals(Conf.formatDate(new
		// Date()),result.get(Conf.CREATION_DATE));
		assertEquals(testChannelJID.toBareJID(), result.get(Conf.OWNER));
		assertEquals(Affiliations.publisher.toString(),
				result.get(Conf.DEFAULT_AFFILIATION));
		assertEquals("1", result.get(Conf.NUM_SUBSCRIBERS));
		assertEquals("1", result.get(Conf.NOTIFY_CONFIG));
		assertEquals("personal", result.get(Conf.CHANNEL_TYPE));
	}

	@Test
	public void testGetStatusChannelNodename() {
		assertEquals("/user/" + this.testChannelJID.toBareJID() + "/status",
				Conf.getStatusChannelNodename(testChannelJID));
	}

	@Test
	public void testGetDefaultStatusChannelConf() {
		HashMap<String, String> result = Conf
				.getDefaultStatusChannelConf(testChannelJID);
		assertEquals(10, result.size());
		assertEquals(this.testChannelJID.toBareJID()
				+ "'s very own buddycloud status!", result.get(Conf.TITLE));
		assertEquals("This is " + this.testChannelJID.toBareJID()
				+ "'s mood a.k.a status -channel. Depends how geek you are.",
				result.get(Conf.DESCRIPTION));
		assertEquals("http://www.w3.org/2005/Atom", result.get(Conf.TYPE));
		assertEquals("publishers", result.get(Conf.PUBLISH_MODEL));
		assertEquals(AccessModels.open.toString(),
				result.get(Conf.ACCESS_MODEL));
		// ToDo: Find a way to test the Date: assertEquals(Conf.formatDate(new
		// Date()),result.get(Conf.CREATION_DATE));
		assertEquals(testChannelJID.toBareJID(), result.get(Conf.OWNER));
		assertEquals(Affiliations.member.toString(),
				result.get(Conf.DEFAULT_AFFILIATION));
		assertEquals("1", result.get(Conf.NUM_SUBSCRIBERS));
		assertEquals("1", result.get(Conf.NOTIFY_CONFIG));
	}

	@Test
	public void testGetGeoPreviousChannelNodename() {
		assertEquals("/user/" + this.testChannelJID.toBareJID()
				+ "/geo/previous",
				Conf.getGeoPreviousChannelNodename(testChannelJID));
	}

	@Test
	public void testGetDefaultGeoPreviousChannelConf() {
		HashMap<String, String> result = Conf
				.getDefaultGeoPreviousChannelConf(testChannelJID);
		assertEquals(10, result.size());
		assertEquals(this.testChannelJID.toBareJID() + "'s previous location.",
				result.get(Conf.TITLE));
		assertEquals("Where " + this.testChannelJID.toBareJID()
				+ " has been before.", result.get(Conf.DESCRIPTION));
		assertEquals("http://www.w3.org/2005/Atom", result.get(Conf.TYPE));
		assertEquals("publishers", result.get(Conf.PUBLISH_MODEL));
		assertEquals(AccessModels.authorize.toString(),
				result.get(Conf.ACCESS_MODEL));
		// ToDo: Find a way to test the Date: assertEquals(Conf.formatDate(new
		// Date()),result.get(Conf.CREATION_DATE));
		assertEquals(testChannelJID.toBareJID(), result.get(Conf.OWNER));
		assertEquals(Affiliations.member.toString(),
				result.get(Conf.DEFAULT_AFFILIATION));
		assertEquals("1", result.get(Conf.NUM_SUBSCRIBERS));
		assertEquals("1", result.get(Conf.NOTIFY_CONFIG));
	}

	@Test
	public void testGetGeoCurrentChannelNodename() {
		assertEquals("/user/" + this.testChannelJID.toBareJID()
				+ "/geo/current",
				Conf.getGeoCurrentChannelNodename(testChannelJID));
	}

	@Test
	public void testGetDefaultGeoCurrentChannelConf() {
		HashMap<String, String> result = Conf
				.getDefaultGeoCurrentChannelConf(testChannelJID);
		assertEquals(10, result.size());
		assertEquals(this.testChannelJID.toBareJID() + "'s current location.",
				result.get(Conf.TITLE));
		assertEquals("Where " + this.testChannelJID.toBareJID() + " is now.",
				result.get(Conf.DESCRIPTION));
		assertEquals("http://www.w3.org/2005/Atom", result.get(Conf.TYPE));
		assertEquals("publishers", result.get(Conf.PUBLISH_MODEL));
		assertEquals(AccessModels.authorize.toString(),
				result.get(Conf.ACCESS_MODEL));
		// ToDo: Find a way to test the Date: assertEquals(Conf.formatDate(new
		// Date()),result.get(Conf.CREATION_DATE));
		assertEquals(testChannelJID.toBareJID(), result.get(Conf.OWNER));
		assertEquals(Affiliations.member.toString(),
				result.get(Conf.DEFAULT_AFFILIATION));
		assertEquals("1", result.get(Conf.NUM_SUBSCRIBERS));
		assertEquals("1", result.get(Conf.NOTIFY_CONFIG));
	}

	@Test
	public void testGetGeoNextChannelNodename() {
		assertEquals("/user/" + this.testChannelJID.toBareJID() + "/geo/next",
				Conf.getGeoNextChannelNodename(testChannelJID));
	}

	@Test
	public void testGetDefaultGeoNextChannelConf() {
		HashMap<String, String> result = Conf
				.getDefaultGeoNextChannelConf(testChannelJID);
		assertEquals(10, result.size());
		assertEquals(this.testChannelJID.toBareJID() + "'s next location.",
				result.get(Conf.TITLE));
		assertEquals("Where " + this.testChannelJID.toBareJID()
				+ " is going to go.", result.get(Conf.DESCRIPTION));
		assertEquals("http://www.w3.org/2005/Atom", result.get(Conf.TYPE));
		assertEquals("publishers", result.get(Conf.PUBLISH_MODEL));
		assertEquals(AccessModels.authorize.toString(),
				result.get(Conf.ACCESS_MODEL));
		// ToDo: Find a way to test the Date: assertEquals(Conf.formatDate(new
		// Date()),result.get(Conf.CREATION_DATE));
		assertEquals(testChannelJID.toBareJID(), result.get(Conf.OWNER));
		assertEquals(Affiliations.member.toString(),
				result.get(Conf.DEFAULT_AFFILIATION));
		assertEquals("1", result.get(Conf.NUM_SUBSCRIBERS));
		assertEquals("1", result.get(Conf.NOTIFY_CONFIG));
	}

	@Test
	public void testGetSubscriptionsChannelNodename() {
		assertEquals("/user/" + this.testChannelJID.toBareJID()
				+ "/subscriptions",
				Conf.getSubscriptionsChannelNodename(testChannelJID));
	}

	@Test
	public void testGetDefaultSubscriptionsChannelConf() {
		HashMap<String, String> result = Conf
				.getDefaultSubscriptionsChannelConf(testChannelJID);
		assertEquals(10, result.size());
		assertEquals(this.testChannelJID.toBareJID() + "'s susbcriptions.",
				result.get(Conf.TITLE));
		assertEquals(this.testChannelJID.toBareJID() + "'s subscriptions. ",
				result.get(Conf.DESCRIPTION));
		assertEquals("http://www.w3.org/2005/Atom", result.get(Conf.TYPE));
		assertEquals("publishers", result.get(Conf.PUBLISH_MODEL));
		assertEquals(AccessModels.open.toString(),
				result.get(Conf.ACCESS_MODEL));
		// ToDo: Find a way to test the Date: assertEquals(Conf.formatDate(new
		// Date()),result.get(Conf.CREATION_DATE));
		assertEquals(testChannelJID.toBareJID(), result.get(Conf.OWNER));
		assertEquals(Affiliations.member.toString(),
				result.get(Conf.DEFAULT_AFFILIATION));
		assertEquals("1", result.get(Conf.NUM_SUBSCRIBERS));
		assertEquals("1", result.get(Conf.NOTIFY_CONFIG));
	}
}