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
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Ignore;
import org.junit.Test;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.ResultSet;

public class JDBCNodeStoreSubscriptionsTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreSubscriptionsTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
	}

	@Test
	public void testAddUserSubscriptionNewSubscription() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed);

		store.addUserSubscription(nodeSubscription);

		dbTester.assertions().assertTableContains("subscriptions",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", TEST_SERVER1_USER1_JID.toString());
						put("subscription", Subscriptions.subscribed.toString());
					}
				});
	}

	@Test
	public void testAddUserSubscriptionNewSubscriptionWithListener()
			throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID,
				TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed);

		store.addUserSubscription(nodeSubscription);

		dbTester.assertions().assertTableContains("subscriptions",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", TEST_SERVER2_USER1_JID.toString());
						put("listener", TEST_SERVER2_CHANNELS_JID.toString());
						put("subscription", Subscriptions.subscribed.toString());
					}
				});
	}

	@Test
	public void testAddUserSubscriptionUpdateSubscriptionWithListener()
			throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID,
				TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed);

		store.addUserSubscription(nodeSubscription);

		dbTester.assertions().assertTableContains("subscriptions",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", TEST_SERVER2_USER1_JID.toString());
						put("listener", TEST_SERVER2_CHANNELS_JID.toString());
						put("subscription", Subscriptions.subscribed.toString());
					}
				});
	}

	@Test
	public void testAddUserSubscriptionWithNoneSubscriptionRemovesSubscription()
			throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.none);

		store.addUserSubscription(nodeSubscription);

		dbTester.assertions().assertTableContains("subscriptions",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", TEST_SERVER1_USER1_JID.toString());
					}
				}, 0);
	}

	@Test
	public void testAddUserSubscriptionWithNoneSubscriptionRemovesSubscriptionIgnoringListener()
			throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, new JID(
						"randomlistener"), Subscriptions.none);

		store.addUserSubscription(nodeSubscription);

		dbTester.assertions().assertTableContains("subscriptions",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", TEST_SERVER1_USER1_JID.toString());
					}
				}, 0);
	}

	@Test(expected = NullPointerException.class)
	public void testAddUserSubscriptionWithNullSubscriptionThrowsException()
			throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, null);

		store.addUserSubscription(nodeSubscription);
	}

	@Test
	public void testAddUserSubscriptionUpdateSubscription() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.unconfigured);

		store.addUserSubscription(nodeSubscription);

		dbTester.assertions().assertTableContains("subscriptions",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", TEST_SERVER1_USER1_JID.toString());
						put("subscription", Subscriptions.subscribed.toString());
					}
				}, 0);

		dbTester.assertions().assertTableContains("subscriptions",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", TEST_SERVER1_USER1_JID.toString());
						put("subscription",
								Subscriptions.unconfigured.toString());
					}
				});
	}

	@Test
	public void testAddUserSubscriptionUsesBareJID() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID_WITH_RESOURCE,
				Subscriptions.subscribed);

		store.addUserSubscription(nodeSubscription);

		dbTester.assertions().assertTableContains("subscriptions",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", TEST_SERVER1_USER1_JID.toString());
						put("subscription", Subscriptions.subscribed.toString());
					}
				});
	}

	@Test
	@Ignore("hsql doesn't like DISTINCT ON")
	public void testGetNodeSubscriptionListeners() throws Exception {
		dbTester.loadData("node_1");

		ResultSet<NodeSubscription> result = store
				.getNodeSubscriptionListeners(TEST_SERVER1_NODE1_ID);

		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {
			{
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER2_JID, Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed));
			}
		};

		assertEquals("Incorrect number of node subscriptions returned",
				expected.size(), result.size());
		assertTrue("Incorrect node subscriptions returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void testCanGetSubscriptionChanges() throws Exception {
		dbTester.loadData("node_1");

		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Subscriptions.subscribed));
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Subscriptions.pending));

		ResultSet<NodeSubscription> changes = store.getSubscriptionChanges(
				TEST_SERVER1_USER1_JID, new Date(0), new Date());
		assertEquals(6, changes.size());
	}

	@Test
	public void testNoSubscriptionChangesFromOutcastNode() throws Exception {
		dbTester.loadData("node_1");

		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Subscriptions.subscribed));
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Subscriptions.pending));
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Affiliations.outcast);

		ResultSet<NodeSubscription> changes = store.getSubscriptionChanges(
				TEST_SERVER1_USER1_JID, new Date(0), new Date());
		assertEquals(0, changes.size());
	}
}