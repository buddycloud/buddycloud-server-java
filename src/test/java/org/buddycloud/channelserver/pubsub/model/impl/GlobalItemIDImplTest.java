package org.buddycloud.channelserver.pubsub.model.impl;

import static org.junit.Assert.*;

import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.JID;

public class GlobalItemIDImplTest {
	@Test
	public void testFromStringWithValidStrings() {
		checkValid("tag:pubsub.server.com,this/is/a/node,this-in-an-item",
				new JID("pubsub.server.com"), "this/is/a/node",
				"this-in-an-item");
		checkValid(
				"tag:test@pubsub.server.com,some kind of RANDOM node ID,4545nnn342300-dsvads=f/fdsafh",
				new JID("test@pubsub.server.com"),
				"some kind of RANDOM node ID", "4545nnn342300-dsvads=f/fdsafh");
	}
	
	/**
	 * This is to handle a case with existing buddycloud clients which send the service as <code>"null@&lt;server&gt;"</code>
	 */
	@Test
	public void testFromStringWithNullNodeInService() {
		checkValid("tag:null@pubsub.server.com,this/is/a/node,this-in-an-item",
				new JID("pubsub.server.com"), "this/is/a/node",
				"this-in-an-item");
	}

	@Test(expected=IllegalArgumentException.class)
	public void testFromStringWithInvalidString() {
		GlobalItemIDImpl.fromString("this isn't valid");
	}

	@Test(expected=IllegalArgumentException.class)
	public void testFromStringWithMissingTag() {
		GlobalItemIDImpl.fromString("pubsub.server.com,this/is/a/node,this-in-an-item");
	}

	@Test
	public void checkEqualsABit() {
		GlobalItemID itemID = new GlobalItemIDImpl(new JID("denmark.lit"), "node", "item-id");
		GlobalItemID itemID2 = new GlobalItemIDImpl(new JID("denmark.lit"), "node", "item-id");
		
		assertEquals("Equals isn't working!", itemID, itemID2);
	}

	private void checkValid(String input, JID service, String nodeID,
			String itemID) {
		GlobalItemID result = GlobalItemIDImpl.fromString(input);

		assertEquals("Incorrect service", service, result.getService());
		assertEquals("Incorrect service", nodeID, result.getNodeID());
		assertEquals("Incorrect service", itemID, result.getItemID());
	}
}
