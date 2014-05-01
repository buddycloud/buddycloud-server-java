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
	public void testGetUserSubscription() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscription result = store.getUserSubscription(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

		NodeSubscriptionImpl expected = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed);

		assertEquals("An unexpected node subscription was returned", expected,
				result);
	}

	@Test
	public void testGetUserSubscriptionIfNotSet() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscription result = store.getUserSubscription(
				TEST_SERVER1_NODE1_ID, new JID("anotheruser@sample.com"));

		NodeSubscriptionImpl expected = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, new JID("anotheruser@sample.com"),
				Subscriptions.none);

		assertEquals("An unexpected node subscription was returned", expected,
				result);
	}

	@Test
	public void testGetUserSubscriptionUsesBareJID() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscription result = store.getUserSubscription(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID_WITH_RESOURCE);

		NodeSubscriptionImpl expected = new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed);

		assertEquals("An unexpected node subscription was returned", expected,
				result);
	}

	@Test
	public void testGetUserSubscriptions() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeSubscription> result = store
				.getUserSubscriptions(TEST_SERVER1_USER1_JID);

		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {
			{
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed));
			}
		};

		assertEquals("Incorrect number of user subscriptions returned",
				expected.size(), result.size());
		assertTrue("Incorrect user subscriptions returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void testGetUserSubscriptionsWithListener() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeSubscription> result = store
				.getUserSubscriptions(TEST_SERVER2_USER1_JID);

		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {
			{
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID,
						TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
			}
		};

		assertEquals("Incorrect number of user subscriptions returned",
				expected.size(), result.size());
		assertTrue("Incorrect user subscriptions returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void testGetUserSubscriptionsForInbox() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeSubscription> result = store
				.getUserSubscriptions(TEST_SERVER2_CHANNELS_JID);

		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {
			{
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID,
						TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER3_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
			}
		};

		assertEquals("Incorrect number of user subscriptions returned",
				expected.size(), result.size());
		assertTrue("Incorrect user subscriptions returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void testGetUserSubscriptionsUsesBareJID() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeSubscription> result = store
				.getUserSubscriptions(TEST_SERVER1_USER1_JID_WITH_RESOURCE);

		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {
			{
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed));
			}
		};

		assertEquals("Incorrect number of user subscriptions returned",
				expected.size(), result.size());
		assertTrue("Incorrect user subscriptions returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void testGetNodeSubscriptions() throws Exception {
		dbTester.loadData("node_1");

		ResultSet<NodeSubscription> result = store
				.getNodeSubscriptions(TEST_SERVER1_NODE1_ID, false);

		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {
			{
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, TEST_SERVER1_USER1_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER2_JID, TEST_SERVER1_USER2_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER3_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
			}
		};

		assertEquals("Incorrect number of node subscriptions returned",
				expected.size(), result.size());
		assertTrue("Incorrect node subscriptions returned",
				CollectionUtils.isEqualCollection(expected, result));
	}


	@Test
	public void testGetNodeSubscriptionsForOwnerModerator() throws Exception {
		dbTester.loadData("node_1");

		ResultSet<NodeSubscription> result = store
				.getNodeSubscriptions(TEST_SERVER1_NODE1_ID, true);

		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {
			{
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, TEST_SERVER1_USER1_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER2_JID, TEST_SERVER1_USER2_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER3_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed));
				add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_OUTCAST_JID, TEST_SERVER1_OUTCAST_JID,
						Subscriptions.subscribed));
			}
		};

		assertEquals("Incorrect number of node subscriptions returned",
				expected.size(), result.size());
		assertTrue("Incorrect node subscriptions returned",
				CollectionUtils.isEqualCollection(expected, result));
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
	public void testGetUserSubscriptionsForUnknownUserReturnsNone()
			throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeSubscription> result = store
				.getUserSubscriptions(new JID("unknown@example.com"));

		assertTrue("Incorrect user subscriptions returned", result.isEmpty());
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

	@Test
	public void testCanGetNodeSubscriptionsWithRsm() throws Exception {
		dbTester.loadData("node_1");

		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Subscriptions.subscribed));
		Thread.sleep(1);
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Subscriptions.subscribed));
		Thread.sleep(1);
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed));
		Thread.sleep(1);
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID,
				Subscriptions.subscribed));

		ResultSet<NodeSubscription> result = store.getNodeSubscriptions(
				TEST_SERVER1_NODE1_ID, false, TEST_SERVER1_USER1_JID, 50);
		ResultSet<NodeSubscription> result1 = store.getNodeSubscriptions(
				TEST_SERVER1_NODE1_ID, false, TEST_SERVER1_USER2_JID, 50);

		ResultSet<NodeSubscription> result2 = store.getNodeSubscriptions(
				TEST_SERVER1_NODE1_ID, false, TEST_SERVER1_USER3_JID, 50);

		assertEquals(1, result.size());
		assertEquals(2, result1.size());
		assertEquals(3, result2.size());
	}

	@Test
	public void testCanGetNodeSubscriptionsForOwnerModeratorWithRsm() throws Exception {
		dbTester.loadData("node_1");

		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Subscriptions.subscribed));
		Thread.sleep(1);
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Subscriptions.subscribed));
		Thread.sleep(1);
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed));
		Thread.sleep(1);
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID,
				Subscriptions.subscribed));

		ResultSet<NodeSubscription> result = store.getNodeSubscriptions(
				TEST_SERVER1_NODE1_ID, true, TEST_SERVER1_USER1_JID, 50);
		ResultSet<NodeSubscription> result1 = store.getNodeSubscriptions(
				TEST_SERVER1_NODE1_ID, true, TEST_SERVER1_USER2_JID, 50);

		ResultSet<NodeSubscription> result2 = store.getNodeSubscriptions(
				TEST_SERVER1_NODE1_ID, true, TEST_SERVER1_USER3_JID, 50);

		assertEquals(1, result.size());
		assertEquals(2, result1.size());
		assertEquals(3, result2.size());
	}
	
	@Test
	public void testCanRetrictNodeSubscriptionsCountWithRsm() throws Exception {

		dbTester.loadData("node_1");
		// dbTester.loadData("node_2");

		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Subscriptions.subscribed));

		ResultSet<NodeSubscription> result = store.getNodeSubscriptions(
				TEST_SERVER1_NODE1_ID, false, TEST_SERVER1_USER1_JID, 1);
		assertEquals(1, result.size());
	}
	
	@Test
	public void testCanRetrictNodeSubscriptionsCountForOwnerModeratorWithRsm() throws Exception {

		dbTester.loadData("node_1");
		// dbTester.loadData("node_2");

		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Subscriptions.subscribed));

		ResultSet<NodeSubscription> result = store.getNodeSubscriptions(
				TEST_SERVER1_NODE1_ID, true, TEST_SERVER1_USER1_JID, 1);
		assertEquals(1, result.size());
	}
	
	@Test
	public void testCanGetCountOfNodeSubscriptions() throws Exception {
		int affiliations = store.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, false);
		assertEquals(0, affiliations);
	}
	
	@Test
	public void testCanGetCountOfNodeSubscriptionsForOwnerModerator() throws Exception {
		int affiliations = store.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, true);
		assertEquals(0, affiliations);
	}

	@Test
	public void testCanGetCountOfNodeSubscriptionsWithResults()
			throws Exception {
		dbTester.loadData("node_1");
		int subscriptions = store.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, false);
		assertEquals(4, subscriptions);
	}

	@Test
	public void testCanGetCountOfNodeSubscriptionsForOwnerModeratorWithResults()
			throws Exception {
		dbTester.loadData("node_1");
		int subscriptions = store.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, true);
		assertEquals(5, subscriptions);
	}
}