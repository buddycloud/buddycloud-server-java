package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;

import org.apache.commons.collections.CollectionUtils;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.ResultSet;

public class JDBCNodeStoreUserMembershipTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreUserMembershipTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
	}

	@Test
	public void getUserMemberships() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeMembership> result = store
				.getUserMemberships(TEST_SERVER1_USER1_JID);

		HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
			{
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, new Date()));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE2_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.publisher,
						new Date()));
			}
		};

		assertEquals("Incorrect number of user memberships returned",
				expected.size(), result.size());
		assertTrue("Incorrect user memberships returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void getUserMembershipsUsesBareJID() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeMembership> result = store
				.getUserMemberships(TEST_SERVER1_USER1_JID_WITH_RESOURCE);

		HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
			{
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, new Date()));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE2_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.publisher,
						new Date()));
			}
		};

		assertEquals("Incorrect number of user memberships returned",
				expected.size(), result.size());
		assertTrue("Incorrect user memberships returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void canGetUserAffiliationsWithRsm() throws Exception {

		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		store.createNode(TEST_SERVER1_USER3_JID, TEST_SERVER1_NODE3_ID,
				new HashMap<String, String>());

		store.setUserAffiliation(TEST_SERVER1_NODE3_ID, TEST_SERVER1_USER1_JID,
				Affiliations.member);
		Thread.sleep(1);
		store.setUserAffiliation(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID,
				Affiliations.member);
		Thread.sleep(1);
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Affiliations.member);

		ResultSet<NodeMembership> result = store.getUserMemberships(
				TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID, 50);

		ResultSet<NodeMembership> result1 = store.getUserMemberships(
				TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE2_ID, 50);

		ResultSet<NodeMembership> result2 = store.getUserMemberships(
				TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE3_ID, 50);

		assertEquals(0, result.size());
		assertEquals(1, result1.size());
		assertEquals(2, result2.size());
	}

	@Test
	public void canRetrictUserMembershipCountWithRsm() throws Exception {

		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID,
				new HashMap<String, String>());
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE2_ID,
				new HashMap<String, String>());
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE3_ID,
				new HashMap<String, String>());

		store.setUserAffiliation(TEST_SERVER1_NODE3_ID, TEST_SERVER1_USER1_JID,
				Affiliations.member);
		Thread.sleep(1);
		store.setUserAffiliation(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID,
				Affiliations.member);
		Thread.sleep(1);
		store.setUserAffiliation(TEST_SERVER1_NODE3_ID, TEST_SERVER1_USER1_JID,
				Affiliations.member);

		ResultSet<NodeMembership> result = store.getUserMemberships(
				TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID, 2);
		assertEquals(2, result.size());
	}
}