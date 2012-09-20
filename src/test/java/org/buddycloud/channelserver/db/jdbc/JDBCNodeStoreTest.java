package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.IOException;
import java.sql.Connection;
import java.sql.Date;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

import org.apache.commons.collections.CollectionUtils;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.exception.ItemNotFoundException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mockito;
import org.xmpp.packet.JID;

@SuppressWarnings("serial")
public class JDBCNodeStoreTest {

	private static final String TEST_SERVER1_NODE1_ID = "users/node1@server1/posts";
	private static final String TEST_SERVER1_NODE2_ID = "users/node2@server1/posts";
	
	private static final String TEST_SERVER1_NODE1_ITEM1_ID = "a1";
	private static final String TEST_SERVER1_NODE1_ITEM1_CONTENT = "Test 1";

	private static final String TEST_SERVER1_NODE1_ITEM2_ID = "a2";
	private static final String TEST_SERVER1_NODE1_ITEM2_CONTENT = "Test 2";
	
	private static final String TEST_SERVER1_NODE1_ITEM3_ID = "a3";
	private static final String TEST_SERVER1_NODE1_ITEM3_CONTENT = "Test 3";
	
	private static final String TEST_SERVER1_NODE1_ITEM4_ID = "a4";
	private static final String TEST_SERVER1_NODE1_ITEM4_CONTENT = "Test 4";
	
	private static final String TEST_SERVER1_NODE1_ITEM5_ID = "a5";
	private static final String TEST_SERVER1_NODE1_ITEM5_CONTENT = "Test 5";

	private static final String TEST_SERVER1_HOSTNAME = "server1";
	private static final String TEST_SERVER2_HOSTNAME = "server2";

	private static final JID TEST_SERVER1_CHANNELS_JID = new JID("channels.server1");
	private static final JID TEST_SERVER2_CHANNELS_JID = new JID("channels.server2");

	private static final JID TEST_SERVER1_USER1_JID = new JID("user1@server1");
	private static final JID TEST_SERVER1_USER2_JID = new JID("user2@server1");

	private static final JID TEST_SERVER2_USER1_JID = new JID("user1@server2");
	private static final JID TEST_SERVER2_USER2_JID = new JID("user2@server2");

	DatabaseTester dbTester;
	Connection conn;

	JDBCNodeStore store;

	public JDBCNodeStoreTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
	}

	@Before
	public void setUp() throws Exception {
		dbTester.initialise();

		store = new JDBCNodeStore(dbTester.getConnection());
	}

	@After
	public void tearDown() throws Exception {
		dbTester.close();
	}

	@Test
	public void testCreateNodeNoConfig() throws Exception {
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID,
				new HashMap<String, String>());

		HashMap<String, Object> expected = new HashMap<String, Object>();

		expected.put("node", TEST_SERVER1_NODE1_ID);

		dbTester.assertions().assertTableContains("nodes", expected);
		dbTester.assertions().assertTableContains("node_config", expected, 0);

		expected = new HashMap<String, Object>();
		expected.put("node", TEST_SERVER1_NODE1_ID);
		expected.put("user", TEST_SERVER1_USER1_JID.toString());
		expected.put("affiliation", "owner");
		dbTester.assertions().assertTableContains("affiliations", expected);
	}

	@Test
	public void testCreateNodeWithConfig() throws Exception {
		HashMap<String, String> config = new HashMap<String, String>();
		config.put("CONFIG1", "Value of CONFIG1");
		config.put("CONFIG2", "Value of CONFIG2");

		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID, config);

		HashMap<String, Object> expected = new HashMap<String, Object>();
		expected.put("node", TEST_SERVER1_NODE1_ID);
		dbTester.assertions().assertTableContains("nodes", expected);

		expected = new HashMap<String, Object>();
		expected.put("node", TEST_SERVER1_NODE1_ID);
		expected.put("user", TEST_SERVER1_USER1_JID.toString());
		expected.put("affiliation", "owner");
		dbTester.assertions().assertTableContains("affiliations", expected);

		expected = new HashMap<String, Object>();
		expected.put("node", TEST_SERVER1_NODE1_ID);
		expected.put("key", "CONFIG1");
		expected.put("value", "Value of CONFIG1");
		dbTester.assertions().assertTableContains("node_config", expected);

		expected = new HashMap<String, Object>();
		expected.put("node", TEST_SERVER1_NODE1_ID);
		expected.put("key", "CONFIG2");
		expected.put("value", "Value of CONFIG2");
		dbTester.assertions().assertTableContains("node_config", expected);
	}

	@Test(expected = NodeStoreException.class)
	public void testCreateDuplicateNodeThrowsException() throws Exception {
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID,
				new HashMap<String, String>());
		store.createNode(TEST_SERVER1_USER2_JID, TEST_SERVER1_NODE1_ID,
				new HashMap<String, String>());
	}

	@Test
	public void testSetNodeConfExistingConf() throws Exception {
		dbTester.loadData("node_1");

		store.setNodeConf(TEST_SERVER1_NODE1_ID, "config1", "updated config1");

		dbTester.assertions().assertTableContains("node_config",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("key", "config1");
						put("value", "updated config1");
					}
				});

		// Make sure the old config isn't there
		dbTester.assertions().assertTableContains("node_config",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("key", "config1");
						put("value", "Value of config1");
					}
				}, 0);
	}

	@Test
	public void testSetNodeConfNewConf() throws Exception {
		dbTester.loadData("node_1");

		store.setNodeConf(TEST_SERVER1_NODE1_ID, "config3", "Value of config3");

		dbTester.assertions().assertTableContains("node_config",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("key", "config3");
						put("value", "Value of config3");
					}
				});
	}

	@Test
	public void testSetUserAffiliationNewAffiliation() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, new JID("user2@example.com"),
				Affiliations.member);

		dbTester.assertions().assertTableContains("affiliations",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", "user2@example.com");
						put("affiliation", Affiliations.member.toString());
					}
				});
	}

	@Test
	public void testSetUserAffiliationUpdateAffiliation() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, new JID("user1@example.com"),
				Affiliations.member);

		dbTester.assertions().assertTableContains("affiliations",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", "user1@example.com");
						put("affiliation", Affiliations.member.toString());
					}
				});

		dbTester.assertions().assertTableContains("affiliations",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", "user1@example.com");
						put("affiliation", Affiliations.owner.toString());
					}
				}, 0);
	}

	@Test
	public void testSetUserAffiliationUpdateAffiliationNoneRemovesAffiliation() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, new JID("user1@example.com"),
				Affiliations.none);
		dbTester.assertions().assertTableContains("affiliations",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", "user1@example.com");
					}
				}, 0);
	}

	@Test
	public void testNodeExistsSuccess() throws Exception {
		dbTester.loadData("node_1");

		assertTrue(
				"nodeExists returned false when there blatantly is a node there",
				store.nodeExists(TEST_SERVER1_NODE1_ID));
	}

	@Test
	public void testNodeExistsFailure() throws Exception {
		dbTester.loadData("node_1");

		assertFalse(
				"nodeExists returned true when there blatantly is not a node there",
				store.nodeExists("node2"));
	}

	@Test
	public void testNodeExistsIsCaseSensitive() throws Exception {
		dbTester.loadData("node_1");

		assertFalse(
				"nodeExists returned true for Node1 when the only node is node1 (note case)",
				store.nodeExists("Node1"));
	}

	@Test
	public void testGetNodeConfValueSuccess() throws Exception {
		dbTester.loadData("node_1");

		assertEquals("Request for config key config1 is incorrect",
				"Value of config1", store.getNodeConfValue(TEST_SERVER1_NODE1_ID, "config1"));
	}

	@Test
	public void testGetNodeConfValueNonExistantReturnsNull() throws Exception {
		dbTester.loadData("node_1");

		assertNull("Request for non existant config key is incorrect",
				store.getNodeConfValue(TEST_SERVER1_NODE1_ID, "nonexistant"));
	}

	@Test
	public void testAddUserSubscriptionNewSubscription() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed);
		
		store.addUserSubscription(nodeSubscription);
		
		dbTester.assertions().assertTableContains("subscriptions", new HashMap<String,Object>() {{
			put("node", TEST_SERVER1_NODE1_ID);
			put("user", TEST_SERVER1_USER1_JID.toString());
			put("subscription", Subscriptions.subscribed.toString());
		}});
	}

	@Test
	public void testAddUserSubscriptionNewSubscriptionWithListener() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed);
		
		store.addUserSubscription(nodeSubscription);
		
		dbTester.assertions().assertTableContains("subscriptions", new HashMap<String,Object>() {{
			put("node", TEST_SERVER1_NODE1_ID);
			put("user", TEST_SERVER2_USER1_JID.toString());
			put("listener", TEST_SERVER2_CHANNELS_JID.toString());
			put("subscription", Subscriptions.subscribed.toString());
		}});
	}

	@Test
	public void testAddUserSubscriptionWithNoneSubscriptionRemovesSubscription() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.none);
		
		store.addUserSubscription(nodeSubscription);
		
		dbTester.assertions().assertTableContains("subscriptions", new HashMap<String,Object>() {{
			put("node", TEST_SERVER1_NODE1_ID);
			put("user", TEST_SERVER1_USER1_JID.toString());
		}}, 0);
	}

	@Test(expected=NullPointerException.class)
	public void testAddUserSubscriptionWithNullSubscriptionThrowsException() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, null);

		store.addUserSubscription(nodeSubscription);
	}

	@Test
	public void testAddUserSubscriptionUpdateSubscription() throws Exception {
		dbTester.loadData("node_1");

		NodeSubscriptionImpl nodeSubscription = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.unconfigured);
		
		store.addUserSubscription(nodeSubscription);
		
		dbTester.assertions().assertTableContains("subscriptions", new HashMap<String,Object>() {{
			put("node", TEST_SERVER1_NODE1_ID);
			put("user", TEST_SERVER1_USER1_JID.toString());
			put("subscription", Subscriptions.subscribed.toString());
		}}, 0);
		
		dbTester.assertions().assertTableContains("subscriptions", new HashMap<String,Object>() {{
			put("node", TEST_SERVER1_NODE1_ID);
			put("user", TEST_SERVER1_USER1_JID.toString());
			put("subscription", Subscriptions.unconfigured.toString());
		}});
	}

	@Test
	public void testGetUserAffiliation() throws Exception {
		dbTester.loadData("node_1");
		
		NodeAffiliation result = store.getUserAfilliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);
		
		NodeAffiliationImpl expected = new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Affiliations.owner);
		
		assertEquals("An unexpected node affiliation was returned", expected, result);
	}

	@Test
	public void testGetUserAffiliationIfNotSet() throws Exception {
		dbTester.loadData("node_1");
		
		NodeAffiliation result = store.getUserAfilliation(TEST_SERVER1_NODE1_ID, new JID("anotheruser@sample.com"));
		
		NodeAffiliationImpl expected = new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID, new JID("anotheruser@sample.com"), Affiliations.none);
		
		assertEquals("An unexpected node affiliation was returned", expected, result);
	}
	
	@Test
	public void testGetUserSubscriptions() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		
		Collection<NodeSubscription> result = store.getUserSubscriptions(TEST_SERVER1_USER1_JID);
		
		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {{
			NodeSubscriptionImpl sub1 = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed);
			add(sub1);
			
			NodeSubscriptionImpl sub2 = new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed);
			add(sub2);
		}};
		
		assertEquals("Incorrect number of user subscriptions returned", expected.size(), result.size());
		assertTrue("Incorrect user subscriptions returned", CollectionUtils.isEqualCollection(expected, result));
	}
	
	@Test
	public void testGetUserSubscriptionsWithListener() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		
		Collection<NodeSubscription> result = store.getUserSubscriptions(TEST_SERVER2_USER1_JID);
		
		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {{
			add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed));
			add(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed));
		}};
		
		assertEquals("Incorrect number of user subscriptions returned", expected.size(), result.size());
		assertTrue("Incorrect user subscriptions returned", CollectionUtils.isEqualCollection(expected, result));
	}
	
	@Test
	public void testGetUserSubscriptionsForInbox() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		
		Collection<NodeSubscription> result = store.getUserSubscriptions(TEST_SERVER2_CHANNELS_JID);
		
		HashSet<NodeSubscription> expected = new HashSet<NodeSubscription>() {{
			add(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed));
			add(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed));
		}};
		
		assertEquals("Incorrect number of user subscriptions returned", expected.size(), result.size());
		assertTrue("Incorrect user subscriptions returned", CollectionUtils.isEqualCollection(expected, result));
	}
	
	@Test
	public void testGetUserSubscriptionsForUnknownUserReturnsNone() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		
		Collection<NodeSubscription> result = store.getUserSubscriptions(new JID("unknown@example.com"));
		
		assertTrue("Incorrect user subscriptions returned", result.isEmpty());
	}
	
	@Test
	public void testGetNodeItems() throws Exception {
		dbTester.loadData("node_1");
		
		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE1_ID);
		
		String[] expectedNodeIds = {
				TEST_SERVER1_NODE1_ITEM5_ID,
				TEST_SERVER1_NODE1_ITEM4_ID,
				TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID,
				TEST_SERVER1_NODE1_ITEM1_ID,
			};
		String[] expectedEntryContent = {
				TEST_SERVER1_NODE1_ITEM5_CONTENT,
				TEST_SERVER1_NODE1_ITEM4_CONTENT,
				TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
				TEST_SERVER1_NODE1_ITEM1_CONTENT,
			};
		
		int i = 0;
		
		while(result.hasNext()) {
			NodeItem item = result.next();
			
			assertEquals("The " + i + " node returned does not have the expected id", expectedNodeIds[i], item.getId());
			assertTrue("The " + i + " node returned does not have the expected content", item.getPayload().contains(expectedEntryContent[i]));
			
			++i;
		}
		
		assertEquals("Too few items returned", expectedNodeIds.length, i);
		
		assertFalse("Too many items were returned", result.hasNext());
	}
	
	@Test
	public void testGetNodeItemsForUnknownNodeReturnsEmptyIterator() throws Exception {
		dbTester.loadData("node_1");
		
		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE2_ID);
		
		assertFalse("Items were returned but none were expected", result.hasNext());
	}
	
	@Test
	public void testGetNodeItemsWithLimits() throws Exception {
		dbTester.loadData("node_1");
		
		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_ITEM1_ID, 3);
		
		String[] expectedNodeIds = {
				TEST_SERVER1_NODE1_ITEM4_ID,
				TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID,
			};
		String[] expectedEntryContent = {
				TEST_SERVER1_NODE1_ITEM4_CONTENT,
				TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
			};
		
		int i = 0;
		
		while(result.hasNext()) {
			NodeItem item = result.next();
			
			assertEquals("The " + i + " node returned does not have the expected id", expectedNodeIds[i], item.getId());
			assertTrue("The " + i + " node returned does not have the expected content", item.getPayload().contains(expectedEntryContent[i]));
			
			++i;
		}
		
		assertEquals("Too few items returned", expectedNodeIds.length, i);
		
		assertFalse("Too many items were returned", result.hasNext());
	}
	
	@Test
	public void testGetNodeItemsWithNegativeOneCountReturnsAllItems() throws Exception {
		dbTester.loadData("node_1");
		
		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE1_ID, null, -1);
		
		String[] expectedNodeIds = {
				TEST_SERVER1_NODE1_ITEM5_ID,
				TEST_SERVER1_NODE1_ITEM4_ID,
				TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID,
				TEST_SERVER1_NODE1_ITEM1_ID,
			};
		String[] expectedEntryContent = {
				TEST_SERVER1_NODE1_ITEM5_CONTENT,
				TEST_SERVER1_NODE1_ITEM4_CONTENT,
				TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
				TEST_SERVER1_NODE1_ITEM1_CONTENT,
			};
		
		int i = 0;
		
		while(result.hasNext()) {
			NodeItem item = result.next();
			
			assertEquals("The " + i + " node returned does not have the expected id", expectedNodeIds[i], item.getId());
			assertTrue("The " + i + " node returned does not have the expected content", item.getPayload().contains(expectedEntryContent[i]));
			
			++i;
		}
		
		assertEquals("Too few items returned", expectedNodeIds.length, i);
		
		assertFalse("Too many items were returned", result.hasNext());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testGetNodeItemsWithInvalidCountThrowsException() throws Exception {
		store.getNodeItems(TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_ITEM1_ID, -2);
	}
	
	@Test
	public void testGetNodeItemsWithUnknownItemReturnsAllItems() throws Exception {
		dbTester.loadData("node_1");
		
		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE1_ID, "randomunknownitemid", 10);
	
		String[] expectedNodeIds = {
				TEST_SERVER1_NODE1_ITEM5_ID,
				TEST_SERVER1_NODE1_ITEM4_ID,
				TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID,
				TEST_SERVER1_NODE1_ITEM1_ID,
			};
		String[] expectedEntryContent = {
				TEST_SERVER1_NODE1_ITEM5_CONTENT,
				TEST_SERVER1_NODE1_ITEM4_CONTENT,
				TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
				TEST_SERVER1_NODE1_ITEM1_CONTENT,
			};
		
		int i = 0;
		
		while(result.hasNext()) {
			NodeItem item = result.next();
			
			assertEquals("The " + i + " node returned does not have the expected id", expectedNodeIds[i], item.getId());
			assertTrue("The " + i + " node returned does not have the expected content", item.getPayload().contains(expectedEntryContent[i]));
			
			++i;
		}
		
		assertEquals("Too few items returned", expectedNodeIds.length, i);
		
		assertFalse("Too many items were returned", result.hasNext());
	}
	
	@Test
	public void testGetNodeItem() throws Exception {
		dbTester.loadData("node_1");

		NodeItem result = store.getNodeItem(TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_ITEM1_ID);
		
		assertEquals("Unexpected Node ID returned", TEST_SERVER1_NODE1_ITEM1_ID, result.getId());
		assertTrue("Unexpected Node content returned", result.getPayload().contains(TEST_SERVER1_NODE1_ITEM1_CONTENT));
	}
	
	@Test
	public void testAddNodeItem() throws Exception {
		dbTester.loadData("node_1");

		final String itemId = "test-item-id";
		final Timestamp updated = new Timestamp(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";
		
		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID, itemId, updated, testContent);
		store.addNodeItem(item);
		
		dbTester.assertions().assertTableContains("items", new HashMap<String,Object>() {{
			put("node", TEST_SERVER1_NODE1_ID);
			put("id", itemId);
			put("updated", updated);
			put("xml", testContent);
		}});
	}
	
	@Test(expected=NodeStoreException.class)
	public void testAddNodeItemWithExistingIdThrowsException() throws Exception {
		dbTester.loadData("node_1");

		final Date updated = new Date(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";
		
		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_ITEM1_ID, updated, testContent);
		store.addNodeItem(item);
	}
	
	@Test
	public void testUpdateNodeItem() throws Exception {
		dbTester.loadData("node_1");
		
		final Timestamp updated = new Timestamp(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";

		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_ITEM1_ID, updated, testContent);
		store.updateNodeItem(item);
		
		dbTester.assertions().assertTableContains("items", new HashMap<String,Object>() {{
			put("node", TEST_SERVER1_NODE1_ID);
			put("id", TEST_SERVER1_NODE1_ITEM1_ID);
			put("updated", updated);
			put("xml", testContent);
		}});
	}
	
	@Test(expected=ItemNotFoundException.class)
	public void testUpdateNodeItemForNonExistantItemThrowsException() throws Exception {
		dbTester.loadData("node_1");
		
		final String itemId = "test-item-id";
		final Timestamp updated = new Timestamp(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";

		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID, itemId, updated, testContent);
		store.updateNodeItem(item);
	}
	
	@Test
	public void testDeleteNodeItem() throws Exception {
		dbTester.loadData("node_1");
		
		store.deleteNodeItemById(TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_ITEM1_ID);
		
		dbTester.assertions().assertTableContains("items", new HashMap<String,Object>() {{
			put("node", TEST_SERVER1_NODE1_ID);
			put("id", TEST_SERVER1_NODE1_ITEM1_ID);
		}}, 0);
	}
	
	@Test(expected=ItemNotFoundException.class)
	public void testDeleteNodeItemForNonExistantItemThrowsException() throws Exception {
		dbTester.loadData("node_1");
		
		store.deleteNodeItemById(TEST_SERVER1_NODE1_ID, "test-item-id");
	}
	
	@Test
	public void testBeginTransaction() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn);
		
		NodeStore.Transaction t = store.beginTransaction();
		
		assertNotNull("Null transaction returned", t);
		
		verify(conn).setAutoCommit(false);
	}
	
	@Test
	public void testCommitTransaction() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn);
		
		NodeStore.Transaction t = store.beginTransaction();
		t.commit();
		
		verify(conn).commit();
		verify(conn).setAutoCommit(true);
	}
	
	@Test
	public void testCloseTransaction() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn);
		
		NodeStore.Transaction t = store.beginTransaction();
		t.close();
		
		verify(conn, never()).commit();
		verify(conn).rollback();
		verify(conn).setAutoCommit(true);
	}
	
	@Test
	public void testCloseOnAlreadyCommittedTransactionDoesntRollback() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn);
		
		NodeStore.Transaction t = store.beginTransaction();
		t.commit();

		t.close();
		
		verify(conn, never()).rollback();
	}
	
	@Test
	public void testNestedTransactionsOnlySetAutoCommitOnce() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn);
		
		store.beginTransaction();
		
		// Make sure setAutoCommit was called
		verify(conn).setAutoCommit(false);

		store.beginTransaction();
		store.beginTransaction();

		// Make sure setAutoCommit was still only called once
		verify(conn).setAutoCommit(false);
	}
	
	@Test
	public void testNestedTransactionsOnlyCallCommitOnOuterTransaction() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn);
		
		InOrder inOrder = inOrder(conn);
		
		NodeStore.Transaction t1 = store.beginTransaction();
		NodeStore.Transaction t2 = store.beginTransaction();
		NodeStore.Transaction t3 = store.beginTransaction();

		t3.commit();
		verify(conn, never()).commit();	// Make sure that commit isn't called until the outer transaction is committed
		
		t2.commit();
		verify(conn, never()).commit();	// Make sure that commit isn't called until the outer transaction is committed
		
		t1.commit();
		
		inOrder.verify(conn).commit(); // Make sure that commit was called
		inOrder.verify(conn).setAutoCommit(true);
	}
	
	@Test(expected=IllegalStateException.class)
	public void testNestedTransactionsWithRollbackInMiddle() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn);
		
		NodeStore.Transaction t1 = store.beginTransaction();
		NodeStore.Transaction t2 = store.beginTransaction();
		NodeStore.Transaction t3 = store.beginTransaction();

		t3.commit();
		t2.close();
		t1.commit();
	}
	
	@Test(expected=IllegalStateException.class)
	public void testNestedTransactionsWithOutOfOrderCommitsThrowsException() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn);
		
		NodeStore.Transaction t1 = store.beginTransaction();
		NodeStore.Transaction t2 = store.beginTransaction();
		NodeStore.Transaction t3 = store.beginTransaction();

		t3.commit();
		t1.commit();	// t1 must not be committed before t2
		t2.commit();
	}
}
