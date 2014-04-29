package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import junit.framework.Assert;

import org.apache.commons.collections.CollectionUtils;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.exception.ItemNotFoundException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.jdbc.JDBCNodeStore.NodeStoreSQLDialect;
import org.buddycloud.channelserver.db.jdbc.dialect.Sql92NodeStoreDialect;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mockito;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.ResultSet;

@SuppressWarnings("serial")
public class JDBCNodeStoreTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
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

	@Test(expected = NullPointerException.class)
	public void testCreateNodeNullOwner() throws Exception {
		store.createNode(null, TEST_SERVER1_NODE1_ID, null);
	}

	@Test(expected = NullPointerException.class)
	public void testCreateNodeNullNodeId() throws Exception {
		store.createNode(TEST_SERVER1_USER1_JID, null, null);
	}

	@Test(expected = NodeStoreException.class)
	public void testCreateDuplicateNodeThrowsException() throws Exception {
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID,
				new HashMap<String, String>());
		store.createNode(TEST_SERVER1_USER2_JID, TEST_SERVER1_NODE1_ID,
				new HashMap<String, String>());
	}

	@Test
	public void testCreateNodeUsesBareJidForOwner() throws Exception {
		store.createNode(TEST_SERVER1_USER1_JID_WITH_RESOURCE,
				TEST_SERVER1_NODE1_ID, new HashMap<String, String>());

		HashMap<String, Object> expected = new HashMap<String, Object>();
		expected.put("node", TEST_SERVER1_NODE1_ID);
		expected.put("user", TEST_SERVER1_USER1_JID.toString());
		expected.put("affiliation", "owner");
		dbTester.assertions().assertTableContains("affiliations", expected);
	}

	@Test
	public void testDeleteNode() throws Exception {

		HashMap<String, Object> expected = new HashMap<String, Object>();
		expected.put("node", TEST_SERVER1_NODE1_ID);
		expected.put("user", TEST_SERVER1_USER1_JID.toString());
		expected.put("affiliation", "owner");

		store.createNode(TEST_SERVER1_USER1_JID_WITH_RESOURCE,
				TEST_SERVER1_NODE1_ID, new HashMap<String, String>());
		dbTester.assertions().assertTableContains("affiliations", expected);

		store.deleteNode(TEST_SERVER1_NODE1_ID);
		dbTester.assertions().assertTableContains("affiliations", expected, 0);
	}

	@Test
	public void testSetUserAffiliationNewAffiliation() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, new JID(
				"user2@example.com"), Affiliations.member);

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

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, new JID(
				"user1@example.com"), Affiliations.member);

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
	public void testSetUserAffiliationUpdateAffiliationNoneRemovesAffiliation()
			throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, new JID(
				"user1@example.com"), Affiliations.none);
		dbTester.assertions().assertTableContains("affiliations",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("user", "user1@example.com");
					}
				}, 0);
	}

	@Test
	public void testSetUserAffiliationUsesBareJID() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, new JID(
				"user2@example.com/resource"), Affiliations.member);

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
				"Value of config1",
				store.getNodeConfValue(TEST_SERVER1_NODE1_ID, "config1"));
	}

	@Test
	public void testGetNodeConfValueNonExistantReturnsNull() throws Exception {
		dbTester.loadData("node_1");

		assertNull("Request for non existant config key is incorrect",
				store.getNodeConfValue(TEST_SERVER1_NODE1_ID, "nonexistant"));
	}

	@Test
	public void testGetNodeConf() throws Exception {
		dbTester.loadData("node_1");

		Map<String, String> result = store.getNodeConf(TEST_SERVER1_NODE1_ID);

		assertEquals("Returned config if incorrect size",
				TEST_SERVER1_NODE1_CONF.size(), result.size());

		for (Entry<String, String> entry : TEST_SERVER1_NODE1_CONF.entrySet()) {
			assertEquals("Result has incorrect entry for " + entry.getKey(),
					entry.getValue(), result.get(entry.getKey()));
		}
	}
	
	@Test
	public void testNodeWithConfigSaysConfigIsCached() throws Exception {
		dbTester.loadData("node_1");
		Assert.assertTrue(store.isCachedNodeConfig(TEST_SERVER1_NODE1_ID));
	}
	
	@Test
	public void testNodeWithoutConfigSaysConfigNotCached() throws Exception {
		Assert.assertFalse(store.isCachedNodeConfig(TEST_SERVER1_NODE1_ID));
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
	public void testGetUserAffiliation() throws Exception {
		dbTester.loadData("node_1");

		NodeAffiliation result = store.getUserAffiliation(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

		NodeAffiliationImpl expected = new NodeAffiliationImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Affiliations.owner, new Date());

		assertEquals("An unexpected node affiliation was returned", expected,
				result);
	}

	@Test
	public void testGetUserAffiliationIfNotSet() throws Exception {
		dbTester.loadData("node_1");

		NodeAffiliation result = store.getUserAffiliation(
				TEST_SERVER1_NODE1_ID, new JID("anotheruser@sample.com"));

		NodeAffiliationImpl expected = new NodeAffiliationImpl(
				TEST_SERVER1_NODE1_ID, new JID("anotheruser@sample.com"),
				Affiliations.none, new Date());

		assertEquals("An unexpected node affiliation was returned", expected,
				result);
	}

	@Test
	public void testGetUserAffiliationUsesBareJID() throws Exception {
		dbTester.loadData("node_1");

		NodeAffiliation result = store.getUserAffiliation(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID_WITH_RESOURCE);

		NodeAffiliationImpl expected = new NodeAffiliationImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Affiliations.owner, new Date());

		assertEquals("An unexpected node affiliation was returned", expected,
				result);
	}

	@Test
	public void testGetUserAffiliations() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeAffiliation> result = store
				.getUserAffiliations(TEST_SERVER1_USER1_JID);

		HashSet<NodeAffiliation> expected = new HashSet<NodeAffiliation>() {
			{
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Affiliations.owner, new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE2_ID,
						TEST_SERVER1_USER1_JID, Affiliations.publisher,
						new Date()));
			}
		};

		assertEquals("Incorrect number of user affiliations returned",
				expected.size(), result.size());
		assertTrue("Incorrect user affiliations returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void testGetUserAffiliationsUsesBareJID() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeAffiliation> result = store
				.getUserAffiliations(TEST_SERVER1_USER1_JID_WITH_RESOURCE);

		HashSet<NodeAffiliation> expected = new HashSet<NodeAffiliation>() {
			{
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Affiliations.owner, new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE2_ID,
						TEST_SERVER1_USER1_JID, Affiliations.publisher,
						new Date()));
			}
		};

		assertEquals("Incorrect number of user affiliations returned",
				expected.size(), result.size());
		assertTrue("Incorrect user affiliations returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void testCanGetUserAffiliationsWithRsm() throws Exception {

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

		ResultSet<NodeAffiliation> result = store.getUserAffiliations(
				TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID, 50);

		ResultSet<NodeAffiliation> result1 = store.getUserAffiliations(
				TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE2_ID, 50);

		ResultSet<NodeAffiliation> result2 = store.getUserAffiliations(
				TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE3_ID, 50);

		assertEquals(0, result.size());
		assertEquals(1, result1.size());
		assertEquals(2, result2.size());
	}

	@Test
	public void testCanRetrictUserAffiliationCountWithRsm() throws Exception {

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

		ResultSet<NodeAffiliation> result = store.getUserAffiliations(
				TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID, 2);
		assertEquals(2, result.size());
	}

	@Test
	public void testCanGetCountOfUserAffiliations() throws Exception {
		int affiliations = store.countUserAffiliations(TEST_SERVER1_USER1_JID);
		assertEquals(0, affiliations);
	}

	@Test
	public void testCanGetCountOfUserAffiliationWithResults() throws Exception {
		dbTester.loadData("node_1");
		int affiliations = store.countUserAffiliations(TEST_SERVER1_USER1_JID);
		assertEquals(1, affiliations);
	}

	@Test
	public void testCanGetAffiliationsChanges() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID,
				Affiliations.publisher);
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Affiliations.publisher);

		ResultSet<NodeAffiliation> changes = store.getAffiliationChanges(
				TEST_SERVER1_USER1_JID, new Date(0), new Date());
		assertEquals(6, changes.size());
	}

	@Test
	public void testNoAffiliationChangesFromOutcastNode() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER1_JID,
				Affiliations.publisher);
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Affiliations.publisher);
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Affiliations.outcast);

		ResultSet<NodeAffiliation> changes = store.getAffiliationChanges(
				TEST_SERVER1_USER1_JID, new Date(0), new Date());
		assertEquals(0, changes.size());
	}

	@Test
	public void testGetNodeAffiliations() throws Exception {
		dbTester.loadData("node_1");

		ResultSet<NodeAffiliation> result = store
				.getNodeAffiliations(TEST_SERVER1_NODE1_ID, false);

		HashSet<NodeAffiliation> expected = new HashSet<NodeAffiliation>() {
			{
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Affiliations.owner, new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER2_JID, Affiliations.publisher,
						new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER1_JID, Affiliations.publisher,
						new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER3_JID, Affiliations.member,
						new Date()));
			}
		};

		assertEquals("Incorrect number of node affiliations returned",
				expected.size(), result.size());
		assertTrue("Incorrect node affiliations returned",
				CollectionUtils.isEqualCollection(expected, result));
	}
	
	@Test
	public void testGetNodeAffiliationsByOwnerModerator() throws Exception {
		dbTester.loadData("node_1");

		ResultSet<NodeAffiliation> result = store
				.getNodeAffiliations(TEST_SERVER1_NODE1_ID, true);

		HashSet<NodeAffiliation> expected = new HashSet<NodeAffiliation>() {
			{
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Affiliations.owner, new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER2_JID, Affiliations.publisher,
						new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER1_JID, Affiliations.publisher,
						new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER3_JID, Affiliations.member,
						new Date()));
				add(new NodeAffiliationImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_OUTCAST_JID, Affiliations.outcast,
						new Date()));
			}
		};

		assertEquals("Incorrect number of node affiliations returned",
				expected.size(), result.size());
		assertTrue("Incorrect node affiliations returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void testCanGetNodeAffiliationsWithRsm() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Affiliations.member);
		Thread.sleep(1);
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Affiliations.member);

		ResultSet<NodeAffiliation> result = store.getNodeAffiliations(
				TEST_SERVER1_NODE1_ID, false, TEST_SERVER1_USER3_JID.toBareJID(), 50);

		ResultSet<NodeAffiliation> result1 = store.getNodeAffiliations(
				TEST_SERVER1_NODE1_ID, false, TEST_SERVER1_USER2_JID.toBareJID(), 50);

		ResultSet<NodeAffiliation> result2 = store.getNodeAffiliations(
				TEST_SERVER1_NODE1_ID, false, TEST_SERVER1_USER1_JID.toBareJID(), 50);

		assertEquals(0, result.size());
		assertEquals(1, result1.size());
		assertEquals(4, result2.size());
	}
	
	@Test
	public void testCanGetNodeAffiliationsForOwnerModeratorWithRsm() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Affiliations.member);
		Thread.sleep(1);
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Affiliations.member);

		ResultSet<NodeAffiliation> result = store.getNodeAffiliations(
				TEST_SERVER1_NODE1_ID, true, TEST_SERVER1_USER3_JID.toBareJID(), 50);

		ResultSet<NodeAffiliation> result1 = store.getNodeAffiliations(
				TEST_SERVER1_NODE1_ID, true, TEST_SERVER1_USER2_JID.toBareJID(), 50);

		ResultSet<NodeAffiliation> result2 = store.getNodeAffiliations(
				TEST_SERVER1_NODE1_ID, true, TEST_SERVER1_USER1_JID.toBareJID(), 50);

		assertEquals(0, result.size());
		assertEquals(1, result1.size());
		assertEquals(5, result2.size());
	}

	@Test
	public void testCanRetrictNodeAffiliationCountWithRsm() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Affiliations.member);
		Thread.sleep(1);
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Affiliations.member);

		ResultSet<NodeAffiliation> result = store.getNodeAffiliations(
				TEST_SERVER1_NODE1_ID, false, TEST_SERVER1_USER3_JID.toBareJID(), 1);
		assertEquals(1, result.size());
	}
	
	@Test
	public void testCanRetrictNodeAffiliationForOwnerModeratorCountWithRsm() throws Exception {
		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER3_JID,
				Affiliations.member);
		Thread.sleep(1);
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID,
				Affiliations.member);

		ResultSet<NodeAffiliation> result = store.getNodeAffiliations(
				TEST_SERVER1_NODE1_ID, true, TEST_SERVER1_USER3_JID.toBareJID(), 1);
		assertEquals(1, result.size());
	}

	@Test
	public void testCanGetCountOfNodeAffiliations() throws Exception {
		int affiliations = store.countNodeAffiliations(TEST_SERVER1_NODE1_ID, false);
		assertEquals(0, affiliations);
	}
	

	@Test
	public void testCanGetCountOfNodeAffiliationsForOwnerModerator() throws Exception {
		int affiliations = store.countNodeAffiliations(TEST_SERVER1_NODE1_ID, true);
		assertEquals(0, affiliations);
	}

	@Test
	public void testCanGetCountOfNodeAffiliationWithResults() throws Exception {
		dbTester.loadData("node_1");
		int affiliations = store.countNodeAffiliations(TEST_SERVER1_NODE1_ID, false);
		assertEquals(4, affiliations);
	}

	@Test
	public void testCanGetCountOfNodeAffiliationForOwnerModeratorWithResults() throws Exception {
		dbTester.loadData("node_1");
		int affiliations = store.countNodeAffiliations(TEST_SERVER1_NODE1_ID, true);
		assertEquals(5, affiliations);
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
	
	@Test
	public void testIsCachedJidForCachedJid() throws Exception {
		dbTester.loadData("node_1");

		boolean result = store.isCachedJID(TEST_SERVER1_USER1_JID);
		assertEquals("Expected JID to be shown as cached", true, result);
	}

	@Test
	public void testIsCachedJidForNonCachedJid() throws Exception {
		dbTester.loadData("node_1");

		boolean result = store.isCachedJID(new JID("anotheruser@sample.com"));
		assertEquals("Expected JID to be shown as not cached", false, result);
	}

	@Test
	public void testGetNodeItems() throws Exception {
		dbTester.loadData("node_1");

		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE1_ID);

		String[] expectedNodeIds = { TEST_SERVER1_NODE1_ITEM5_ID,
				TEST_SERVER1_NODE1_ITEM4_ID, TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID, TEST_SERVER1_NODE1_ITEM1_ID, };
		String[] expectedEntryContent = { TEST_SERVER1_NODE1_ITEM5_CONTENT,
				TEST_SERVER1_NODE1_ITEM4_CONTENT,
				TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
				TEST_SERVER1_NODE1_ITEM1_CONTENT, };

		int i = 0;

		while (result.hasNext()) {
			NodeItem item = result.next();

			assertEquals("The " + i
					+ " node returned does not have the expected id",
					expectedNodeIds[i], item.getId());
			assertTrue("The " + i
					+ " node returned does not have the expected content", item
					.getPayload().contains(expectedEntryContent[i]));

			++i;
		}

		assertEquals("Too few items returned", expectedNodeIds.length, i);

		assertFalse("Too many items were returned", result.hasNext());
	}

	@Test
	public void testGetNodeItemsForUnknownNodeReturnsEmptyIterator()
			throws Exception {
		dbTester.loadData("node_1");

		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE2_ID);

		assertFalse("Items were returned but none were expected",
				result.hasNext());
	}

	@Test
	public void testGetNodeItemsWithLimits() throws Exception {
		dbTester.loadData("node_1");

		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE1_ID,
				TEST_SERVER1_NODE1_ITEM4_ID, 4);

		String[] expectedNodeIds = { TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID, TEST_SERVER1_NODE1_ITEM1_ID };
		String[] expectedEntryContent = { TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
				TEST_SERVER1_NODE1_ITEM1_CONTENT };

		int i = 0;

		while (result.hasNext()) {
			NodeItem item = result.next();

			assertEquals("The " + i
					+ " node returned does not have the expected id",
					expectedNodeIds[i], item.getId());
			assertTrue("The " + i
					+ " node returned does not have the expected content", item
					.getPayload().contains(expectedEntryContent[i]));

			++i;
		}

		assertEquals("Too few items returned", expectedNodeIds.length, i);

		assertFalse("Too many items were returned", result.hasNext());
	}

	@Test
	public void testGetNodeItemsWithPaging() throws Exception {
		dbTester.loadData("node_1");

		long start = System.currentTimeMillis();

		NodeItem[] items = new NodeItem[20];

		for (int i = 0; i < 20; i++) {
			items[i] = new NodeItemImpl(TEST_SERVER1_NODE1_ID,
					String.valueOf(i), new Date(start + i * 10), "payload"
							+ String.valueOf(i));
			store.addNodeItem(items[i]);
		}

		CloseableIterator<NodeItem> result = store.getNodeItems(
				TEST_SERVER1_NODE1_ID, "15", 3);

		assertEquals("Incorrect node item returned", items[14], result.next());
		assertEquals("Incorrect node item returned", items[13], result.next());
		assertEquals("Incorrect node item returned", items[12], result.next());
	}

	@Test
	public void testGetNodeItemsWithNegativeOneCountReturnsAllItems()
			throws Exception {
		dbTester.loadData("node_1");

		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE1_ID,
				null, -1);

		String[] expectedNodeIds = { TEST_SERVER1_NODE1_ITEM5_ID,
				TEST_SERVER1_NODE1_ITEM4_ID, TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID, TEST_SERVER1_NODE1_ITEM1_ID, };
		String[] expectedEntryContent = { TEST_SERVER1_NODE1_ITEM5_CONTENT,
				TEST_SERVER1_NODE1_ITEM4_CONTENT,
				TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
				TEST_SERVER1_NODE1_ITEM1_CONTENT, };

		int i = 0;

		while (result.hasNext()) {
			NodeItem item = result.next();

			assertEquals("The " + i
					+ " node returned does not have the expected id",
					expectedNodeIds[i], item.getId());
			assertTrue("The " + i
					+ " node returned does not have the expected content", item
					.getPayload().contains(expectedEntryContent[i]));

			++i;
		}

		assertEquals("Too few items returned", expectedNodeIds.length, i);

		assertFalse("Too many items were returned", result.hasNext());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testGetNodeItemsWithInvalidCountThrowsException()
			throws Exception {
		store.getNodeItems(TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_ITEM1_ID,
				-2);
	}

	@Test
	public void testGetNodeItemsWithUnknownItemReturnsAllItems()
			throws Exception {
		dbTester.loadData("node_1");

		Iterator<NodeItem> result = store.getNodeItems(TEST_SERVER1_NODE1_ID,
				"randomunknownitemid", 10);

		String[] expectedNodeIds = { TEST_SERVER1_NODE1_ITEM5_ID,
				TEST_SERVER1_NODE1_ITEM4_ID, TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID, TEST_SERVER1_NODE1_ITEM1_ID, };
		String[] expectedEntryContent = { TEST_SERVER1_NODE1_ITEM5_CONTENT,
				TEST_SERVER1_NODE1_ITEM4_CONTENT,
				TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
				TEST_SERVER1_NODE1_ITEM1_CONTENT, };

		int i = 0;

		while (result.hasNext()) {
			NodeItem item = result.next();

			assertEquals("The " + i
					+ " node returned does not have the expected id",
					expectedNodeIds[i], item.getId());
			assertTrue("The " + i
					+ " node returned does not have the expected content", item
					.getPayload().contains(expectedEntryContent[i]));

			++i;
		}

		assertEquals("Too few items returned", expectedNodeIds.length, i);

		assertFalse("Too many items were returned", result.hasNext());
	}

	@Test
	public void testCountNodeItems() throws Exception {
		dbTester.loadData("node_1");

		int result = store.countNodeItems(TEST_SERVER1_NODE1_ID);

		assertEquals("Incorrect item count", 5, result);
	}

	@Test
	public void testGetNewNodeItemsForUserBetweenDates() throws Exception {

		dbTester.loadData("node_1");

		// We shouldn't see this item come out!
		store.addRemoteNode(TEST_SERVER2_NODE1_ID);
		store.addNodeItem(new NodeItemImpl(TEST_SERVER2_NODE1_ID, "1",
				new Date(), "item-payload"));

		Iterator<NodeItem> result = store.getNewNodeItemsForUser(
				TEST_SERVER1_USER1_JID, new Date(0), new Date());

		String[] expectedNodeIds = { TEST_SERVER1_NODE1_ITEM5_ID,
				TEST_SERVER1_NODE1_ITEM4_ID, TEST_SERVER1_NODE1_ITEM3_ID,
				TEST_SERVER1_NODE1_ITEM2_ID, TEST_SERVER1_NODE1_ITEM1_ID, };
		String[] expectedEntryContent = { TEST_SERVER1_NODE1_ITEM5_CONTENT,
				TEST_SERVER1_NODE1_ITEM4_CONTENT,
				TEST_SERVER1_NODE1_ITEM3_CONTENT,
				TEST_SERVER1_NODE1_ITEM2_CONTENT,
				TEST_SERVER1_NODE1_ITEM1_CONTENT, };

		int i = 0;

		while (result.hasNext()) {
			NodeItem item = result.next();

			assertEquals("The " + i
					+ " node returned does not have the expected id",
					expectedNodeIds[i], item.getId());
			assertTrue("The " + i
					+ " node returned does not have the expected content", item
					.getPayload().contains(expectedEntryContent[i]));

			++i;
		}

		assertEquals("Too few items returned", expectedNodeIds.length, i);
		assertFalse("Too many items were returned", result.hasNext());
	}

	@Test
	public void testGetNewNodeItemsForUserBetweenDatesWhenOutcast()
			throws Exception {

		dbTester.loadData("node_1");

		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Affiliations.outcast);

		Iterator<NodeItem> result = store.getNewNodeItemsForUser(
				TEST_SERVER1_USER1_JID, new Date(0), new Date());

		int i = 0;
		while (result.hasNext())
			++i;

		assertEquals(0, i);
	}

	@Test
	public void testCountNodeItemsNonExistantNode() throws Exception {
		dbTester.loadData("node_1");

		int result = store.countNodeItems("iamanodewhichdoesntexist");

		assertEquals("Incorrect item count", 0, result);
	}

	@Test
	public void testIsCachedNode() throws Exception {
		dbTester.loadData("node_1");

		boolean result = store.isCachedNode(TEST_SERVER1_NODE1_ID);

		assertEquals("Incorrect caching reported", true, result);
	}

	@Test
	public void testIsCachedNodeForNonCachedNode() throws Exception {
		dbTester.loadData("node_1");
		boolean result = store.isCachedNode("iamanodewhichdoesntexist");

		assertEquals("Incorrect cached response", false, result);
	}

	@Test
	public void testGetNodeItem() throws Exception {
		dbTester.loadData("node_1");

		NodeItem result = store.getNodeItem(TEST_SERVER1_NODE1_ID,
				TEST_SERVER1_NODE1_ITEM1_ID);

		assertEquals("Unexpected Node ID returned",
				TEST_SERVER1_NODE1_ITEM1_ID, result.getId());
		assertTrue("Unexpected Node content returned", result.getPayload()
				.contains(TEST_SERVER1_NODE1_ITEM1_CONTENT));
	}

	@Test
	public void testCanGetItemReplies() throws Exception {
		dbTester.loadData("node_1");
		NodeItem testItem = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a6",
				new Date(), "<entry>payload</entry>", "a5");
		store.addNodeItem(testItem);

		ClosableIteratorImpl<NodeItem> items = store.getNodeItemReplies(
				TEST_SERVER1_NODE1_ID, "a5", null, -1);

		int count = 0;
		NodeItem item = null;
		while (items.hasNext()) {
			++count;
			item = items.next();

		}
		assertEquals(1, count);
		assertSameNodeItem(item, testItem);
	}

	@Test
	public void testCanGetItemRepliesWithResultSetManagement() throws Exception {
		dbTester.loadData("node_1");
		NodeItem testItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a6",
				new Date(), "<entry>payload</entry>", "a5");
		NodeItem testItem2 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a7",
				new Date(0), "<entry>payload</entry>", "a5");
		NodeItem testItem3 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a8",
				new Date(100), "<entry>payload</entry>", "a5");
		NodeItem testItem4 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a9",
				new Date(200), "<entry>payload</entry>", "/full-node-item-id-ref/a5");
		store.addNodeItem(testItem1);
		store.addNodeItem(testItem2);
		store.addNodeItem(testItem3);
		store.addNodeItem(testItem4);

		ClosableIteratorImpl<NodeItem> items = store.getNodeItemReplies(
				TEST_SERVER1_NODE1_ID, "a5", "a7", 2);

		int count = 0;
		ArrayList<NodeItem> itemsResult = new ArrayList<NodeItem>();
		while (items.hasNext()) {
			++count;
			itemsResult.add(items.next());

		}
		assertEquals(2, count);
		assertSameNodeItem(itemsResult.get(0), testItem4);
		assertSameNodeItem(itemsResult.get(1), testItem1);
	}

	@Test
	public void testCanGetCountOfItemReplies() throws Exception {
		dbTester.loadData("node_1");
		NodeItem testItem = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a6",
				new Date(), "<entry>payload</entry>", "/full-node-item-id-ref/a5");
		store.addNodeItem(testItem);

		int items = store.getCountNodeItemReplies(TEST_SERVER1_NODE1_ID, "a5");
		assertEquals(1, items);
	}

	@Test
	public void testCanGetItemThread() throws Exception {
		dbTester.loadData("node_1");
		NodeItem testItem = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a6",
				new Date(), "<entry>payload</entry>", "/full-node-item-id-ref/a5");
		store.addNodeItem(testItem);

		ClosableIteratorImpl<NodeItem> items = store.getNodeItemThread(
				TEST_SERVER1_NODE1_ID, "a5", null, -1);

		int count = 0;
		NodeItem item = null;
		while (items.hasNext()) {
			++count;
			item = items.next();

		}
		assertEquals(2, count);
		assertSameNodeItem(item, testItem);
	}
	

	@Test
	public void testCanGetItemThreadWithResultSetManagement() throws Exception {
		dbTester.loadData("node_1");
		NodeItem testItemParent = new NodeItemImpl(TEST_SERVER1_NODE1_ID,
				"a100", new Date(100), "<entry>payload parent</entry>");
		NodeItem testItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a6",
				new Date(20), "<entry>payload</entry>", "a100");
		NodeItem testItem2 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a7",
				new Date(40), "<entry>payload</entry>", "a100");
		NodeItem testItem3 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "/full-node-item-id-ref/a8",
				new Date(80), "<entry>payload</entry>", "a100");
		NodeItem testItem4 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a9",
				new Date(160), "<entry>payload</entry>", "a100");
		store.addNodeItem(testItemParent);
		store.addNodeItem(testItem1);
		store.addNodeItem(testItem2);
		store.addNodeItem(testItem3);
		store.addNodeItem(testItem4);

		ClosableIteratorImpl<NodeItem> items = store.getNodeItemThread(
				TEST_SERVER1_NODE1_ID, "a100", "a7", 2);

		int count = 0;
		ArrayList<NodeItem> itemsResult = new ArrayList<NodeItem>();
		while (items.hasNext()) {
			++count;
			itemsResult.add(items.next());

		}
		assertEquals(2, count);
		assertSameNodeItem(itemsResult.get(0), testItemParent);
		assertSameNodeItem(itemsResult.get(1), testItem4);
	}

	@Test
	public void testCanGetCountOfItemThread() throws Exception {
		dbTester.loadData("node_1");
		NodeItem testItem = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a6",
				new Date(), "<entry>payload</entry>", "/full-node-item-id-ref/a5");
		store.addNodeItem(testItem);

		int items = store.getCountNodeThread(TEST_SERVER1_NODE1_ID, "a5");
		assertEquals(2, items);
	}

	@Test
	public void testAddNodeItem() throws Exception {
		dbTester.loadData("node_1");

		final String itemId = "test-item-id";
		final Timestamp updated = new Timestamp(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";

		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID, itemId,
				updated, testContent);
		store.addNodeItem(item);

		dbTester.assertions().assertTableContains("items",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("id", itemId);
						put("updated", updated);
						put("xml", testContent);
					}
				});
	}

	@Test
	public void testAddNoteItemWithInReplyTo() throws Exception {
		dbTester.loadData("node_1");

		final String itemId = "test-item-id";
		final Timestamp updated = new Timestamp(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";
		final String inReplyTo = "a5";

		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID, itemId,
				updated, testContent, "a5");
		store.addNodeItem(item);

		dbTester.assertions().assertTableContains("items",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("id", itemId);
						put("updated", updated);
						put("xml", testContent);
						put("in_reply_to", inReplyTo);
					}
				});
	}

	@Test(expected = NodeStoreException.class)
	public void testAddNodeItemWithExistingIdThrowsException() throws Exception {
		dbTester.loadData("node_1");

		final Date updated = new Date(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";

		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID,
				TEST_SERVER1_NODE1_ITEM1_ID, updated, testContent);
		store.addNodeItem(item);
	}

	@Test
	public void testUpdateNodeItem() throws Exception {
		dbTester.loadData("node_1");

		final Timestamp updated = new Timestamp(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";

		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID,
				TEST_SERVER1_NODE1_ITEM1_ID, updated, testContent);
		store.updateNodeItem(item);

		dbTester.assertions().assertTableContains("items",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("id", TEST_SERVER1_NODE1_ITEM1_ID);
						put("updated", updated);
						put("xml", testContent);
					}
				});
	}

	@Test(expected = ItemNotFoundException.class)
	public void testUpdateNodeItemForNonExistantItemThrowsException()
			throws Exception {
		dbTester.loadData("node_1");

		final String itemId = "test-item-id";
		final Timestamp updated = new Timestamp(System.currentTimeMillis());
		final String testContent = "<content>Hello World</content>";

		NodeItem item = new NodeItemImpl(TEST_SERVER1_NODE1_ID, itemId,
				updated, testContent);
		store.updateNodeItem(item);
	}

	@Test
	public void testDeleteNodeItem() throws Exception {
		dbTester.loadData("node_1");

		store.deleteNodeItemById(TEST_SERVER1_NODE1_ID,
				TEST_SERVER1_NODE1_ITEM1_ID);

		dbTester.assertions().assertTableContains("items",
				new HashMap<String, Object>() {
					{
						put("node", TEST_SERVER1_NODE1_ID);
						put("id", TEST_SERVER1_NODE1_ITEM1_ID);
					}
				}, 0);
	}

	@Test(expected = ItemNotFoundException.class)
	public void testDeleteNodeItemForNonExistantItemThrowsException()
			throws Exception {

		dbTester.loadData("node_1");

		store.deleteNodeItemById(TEST_SERVER1_NODE1_ID, "test-item-id");
	}

	@Test
	public void testGetNodeListReturnsExpectedNodes() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		assertEquals(2, store.getNodeList().size());
	}

	@Test
	public void testPurgeNodeItemsDeletesNodeItems() throws Exception {

		dbTester.loadData("node_1");
		assertTrue(store.countNodeItems(TEST_SERVER1_NODE1_ID) > 0);
		store.purgeNodeItems(TEST_SERVER1_NODE1_ID);
		assertEquals(0, store.countNodeItems(TEST_SERVER1_NODE1_ID));
	}

	@Test
	public void testPurgeNodeItemsDoesntDeleteItemsUnexpectedly()
			throws Exception {

		dbTester.loadData("node_1");
		int itemCount = store.countNodeItems(TEST_SERVER1_NODE1_ID);
		assertTrue(itemCount > 0);
		store.purgeNodeItems(TEST_SERVER1_NODE2_ID); // <--- NODE **2**
		assertEquals(itemCount, store.countNodeItems(TEST_SERVER1_NODE1_ID));
	}

	@Test
	public void testGetNodeSubscriptionCountReturnsZeroWhereThereAreNone()
			throws Exception {
		int subscriptionCount = store
				.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, false);
		assertEquals(0, subscriptionCount);
	}

	@Test
	public void testGetNodeSubscriptionCountReturnsZeroForOwnerModeratorWhereThereAreNone()
			throws Exception {
		int subscriptionCount = store
				.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, true);
		assertEquals(0, subscriptionCount);
	}
	
	@Test
	public void testGetNodeSubscriptionCountReturnsResultWhereThereAreSome()
			throws Exception {
		dbTester.loadData("node_1");
		int subscriptionCount = store
				.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, false);
		assertEquals(4, subscriptionCount);
	}
	
	@Test
	public void testGetNodeSubscriptionCountForOwnerModeratorReturnsResultWhereThereAreSome()
			throws Exception {
		dbTester.loadData("node_1");
		int subscriptionCount = store
				.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, true);
		assertEquals(5, subscriptionCount);
	}
	
	@Test
	public void testGetIsCachedSubscriptionNodeReturnsFalseWhereThereAreNoSubscriptions()
			throws Exception {
		boolean cached = store.nodeHasSubscriptions(TEST_SERVER1_NODE1_ID);
		assertEquals(false, cached);
	}

	@Test
	public void testGetIsCachedSubscriptionNodeReturnsTrueWhereThereAreSubscriptions()
			throws Exception {
		dbTester.loadData("node_1");
		int subscriptionCount = store
				.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, false);
		boolean cached = store.nodeHasSubscriptions(TEST_SERVER1_NODE1_ID);
		assertEquals(true, cached);
	}

	@Test
	public void testGetIsCachedSubscriptionNodeForOwnerModeratorReturnsTrueWhereThereAreSubscriptions()
			throws Exception {
		dbTester.loadData("node_1");
		int subscriptionCount = store
				.countNodeSubscriptions(TEST_SERVER1_NODE1_ID, true);
		boolean cached = store.nodeHasSubscriptions(TEST_SERVER1_NODE1_ID);
		assertEquals(true, cached);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testFirehoseItemsThrowsExceptionIfNegativeLimitRequested()
			throws Exception {
		store.getFirehose(-1, null, false);
	}

	@Test
	public void testCanGetFirehoseItems() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, "pubsub#access_model",
				"open");
		store.setNodeConfValue(TEST_SERVER1_NODE2_ID, "pubsub#access_model",
				"open");
		// Add a remote node - don't expect to see
		HashMap<String, String> remoteNodeConf = new HashMap<String, String>();
		remoteNodeConf.put("pubsub#access_model", "open");
		store.addRemoteNode(TEST_SERVER2_NODE1_ID);
		store.setNodeConf(TEST_SERVER2_NODE1_ID, remoteNodeConf);
		// Add a private node - don't expect to see
		HashMap<String, String> privateNodeConf = new HashMap<String, String>();
		privateNodeConf.put("pubsub#access_model", "subscribe");
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE3_ID,
				privateNodeConf);
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE3_ID, "1111",
				new Date(), "<entry/>"));
		Thread.sleep(4);
		CloseableIterator<NodeItem> items = store.getFirehose(50, null, false);
		NodeItem item = null;
		int count = 0;
		while (items.hasNext()) {
			item = items.next();
			++count;
		}
		assertEquals(6, count);
	}

	@Test
	public void testCanGetFirehoseItemsIncludingPrivateAsAdminUser()
			throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, AccessModel.FIELD_NAME,
				AccessModels.open.toString());
		store.setNodeConfValue(TEST_SERVER1_NODE2_ID, AccessModel.FIELD_NAME,
				AccessModels.open.toString());
		// Add a private node - *do* expect to see
		HashMap<String, String> privateNodeConf = new HashMap<String, String>();
		privateNodeConf.put(AccessModel.FIELD_NAME, AccessModels.authorize.toString());
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE3_ID,
				privateNodeConf);
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE3_ID, "1111",
				new Date(), "<entry/>"));
		// Add a remote node - don't expect to see
		HashMap<String, String> remoteNodeConf = new HashMap<String, String>();
		remoteNodeConf.put(AccessModel.FIELD_NAME, AccessModels.open.toString());
		store.addRemoteNode(TEST_SERVER2_NODE1_ID);
		store.setNodeConf(TEST_SERVER2_NODE1_ID, remoteNodeConf);

		CloseableIterator<NodeItem> items = store.getFirehose(50, null, true);
		NodeItem item = null;
		int count = 0;
		while (items.hasNext()) {
			item = items.next();
			++count;
		}
		assertEquals(7, count);
	}

	@Test
	@Ignore("Ordering by timestamp isn't happening here. Return to later")
	public void testCanGetFirehoseItemsWithRsm() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, "pubsub#access_model",
				"open");
		store.setNodeConfValue(TEST_SERVER1_NODE2_ID, "pubsub#access_model",
				"open");
		// Add a private node - don't expect to see
		HashMap<String, String> privateNodeConf = new HashMap<String, String>();
		privateNodeConf.put("pubsub#access_model", "subscribe");
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE3_ID,
				privateNodeConf);
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE3_ID, "1111",
				new Date(), "<entry/>"));

		// Add a remote node - don't expect to see
		HashMap<String, String> remoteNodeConf = new HashMap<String, String>();
		remoteNodeConf.put("pubsub#access_model", "open");
		store.addRemoteNode(TEST_SERVER2_NODE1_ID);
		store.setNodeConf(TEST_SERVER2_NODE1_ID, remoteNodeConf);

		CloseableIterator<NodeItem> items = store.getFirehose(2, "a3", false);
		NodeItem item1 = items.next();
		NodeItem item2 = items.next();
		assertFalse(items.hasNext());
		assertEquals("a4", item1.getId());
		assertEquals("node2:1", item2.getId());
		assertEquals(TEST_SERVER1_NODE1_ID, item1.getNodeId());
		assertEquals(TEST_SERVER1_NODE2_ID, item2.getNodeId());
	}

	@Test
	public void testCanGetFirehoseItemCount() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, "pubsub#access_model",
				"open");
		store.setNodeConfValue(TEST_SERVER1_NODE2_ID, "pubsub#access_model",
				"open");
		// Add a private node - *do* expect to see
		HashMap<String, String> privateNodeConf = new HashMap<String, String>();
		privateNodeConf.put("pubsub#access_model", "subscribe");
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE3_ID,
				privateNodeConf);
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE3_ID, "1111",
				new Date(), "<entry/>"));
		// Add a remote node - don't expect to see
		HashMap<String, String> remoteNodeConf = new HashMap<String, String>();
		remoteNodeConf.put("pubsub#access_model", "open");
		store.addRemoteNode(TEST_SERVER2_NODE1_ID);
		store.setNodeConf(TEST_SERVER2_NODE1_ID, remoteNodeConf);
		assertEquals(6, store.getFirehoseItemCount(false));
	}

	@Test
	public void testCanGetFirehostItemCountWithPrivateItemsAsAdmin()
			throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, "pubsub#access_model",
				"open");
		store.setNodeConfValue(TEST_SERVER1_NODE2_ID, "pubsub#access_model",
				"open");
		// Add a private node - *do* expect to see
		HashMap<String, String> privateNodeConf = new HashMap<String, String>();
		privateNodeConf.put("pubsub#access_model", "subscribe");
		store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE3_ID,
				privateNodeConf);
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE3_ID, "1111",
				new Date(), "<entry/>"));
		// Add a remote node - don't expect to see
		HashMap<String, String> remoteNodeConf = new HashMap<String, String>();
		remoteNodeConf.put("pubsub#access_model", "open");
		store.addRemoteNode(TEST_SERVER2_NODE1_ID);
		store.setNodeConf(TEST_SERVER2_NODE1_ID, remoteNodeConf);
		assertEquals(7, store.getFirehoseItemCount(true));
	}
	
	@Test
	public void testOnlySeeSearchResultsFromSubscribedPostsNodes() throws Exception {
	    dbTester.loadData("search-test-1");
	    CloseableIterator<NodeItem> items = store.performSearch(
	        new JID("user1@server1"), new ArrayList<String>(), new JID("author@server1"), 1, 25
	    );
	    int counter = 0;
	    while (items.hasNext()) {
	    	++counter;
	    	NodeItem item = items.next();
	        assertEquals("a1", item.getId());
	        assertEquals("/users/subscribed@server1/posts", item.getNodeId());
	    }
	    assertEquals(1, counter);
	}
	
	@Test
	public void testOnlySeeSearchResultsFromRequestedAuthor() throws Exception {
	    dbTester.loadData("search-test-2");
	    CloseableIterator<NodeItem> items = store.performSearch(
	        new JID("user1@server1"), new ArrayList<String>(), new JID("author@server1"), 1, 25
	    );
	    int counter = 0;
	    NodeItem item;
	    while (items.hasNext()) {
	    	++counter;
	    	item = items.next();
	    	if (1 == counter) {
	    		assertEquals("b1", item.getId());
	            assertEquals("/users/another-subscribed@server1/posts", item.getNodeId());
	    	} else if (2 == counter) {
		        assertEquals("a1", item.getId());
		        assertEquals("/users/subscribed@server1/posts", item.getNodeId());
	    	}
	    }
	    assertEquals(2, counter);
	}
	
	@Test
	public void testOnlySeeSearchResultsWithSpecificContent() throws Exception {
	    dbTester.loadData("search-test-3");
	    
	    ArrayList<String> searchTerms = new ArrayList<String>();
	    searchTerms.add("keyword");
	    searchTerms.add("post");
	    
	    CloseableIterator<NodeItem> items = store.performSearch(
	        new JID("user1@server1"), searchTerms, new JID("author@server1"), 1, 25
	    );
	    int counter = 0;
	    NodeItem item;
	    while (items.hasNext()) {
	    	++counter;
	    	item = items.next();
	    	if (1 == counter) {
	    		assertEquals("a3", item.getId());
	            assertEquals("/users/subscribed@server1/posts", item.getNodeId());
	    	} else if (2 == counter) {
		        assertEquals("a1", item.getId());
		        assertEquals("/users/subscribed@server1/posts", item.getNodeId());
	    	}
	    }
	    assertEquals(2, counter);
	}
	
	@Test
	public void testOnlySeeSearchResultsWithSpecificContentAndAuthor() throws Exception {
	    dbTester.loadData("search-test-4");
	    
	    ArrayList<String> searchTerms = new ArrayList<String>();
	    searchTerms.add("keyword");
	    searchTerms.add("post");
	    
	    CloseableIterator<NodeItem> items = store.performSearch(
	        new JID("user1@server1"), searchTerms, new JID("author@server1"), 1, 25
	    );
	    int counter = 0;
	    NodeItem item;
	    while (items.hasNext()) {
	    	++counter;
	    	item = items.next();
	    	if (1 == counter) {
	    		assertEquals("a3", item.getId());
	            assertEquals("/users/subscribed@server1/posts", item.getNodeId());
	    	} else if (2 == counter) {
		        assertEquals("a1", item.getId());
		        assertEquals("/users/subscribed@server1/posts", item.getNodeId());
	    	}
	    }
	    assertEquals(2, counter);
	}

	@Test
	public void testBeginTransaction() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn,
				mock(NodeStoreSQLDialect.class));

		NodeStore.Transaction t = store.beginTransaction();

		assertNotNull("Null transaction returned", t);

		verify(conn).setAutoCommit(false);
	}

	@Test
	public void testCommitTransaction() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn,
				mock(NodeStoreSQLDialect.class));

		NodeStore.Transaction t = store.beginTransaction();
		t.commit();

		verify(conn).commit();
		verify(conn).setAutoCommit(true);
	}

	@Test
	public void testCloseTransaction() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn,
				mock(NodeStoreSQLDialect.class));

		NodeStore.Transaction t = store.beginTransaction();
		t.close();

		verify(conn, never()).commit();
		verify(conn).rollback();
		verify(conn).setAutoCommit(true);
	}

	@Test
	public void testCloseOnAlreadyCommittedTransactionDoesntRollback()
			throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn,
				mock(NodeStoreSQLDialect.class));

		NodeStore.Transaction t = store.beginTransaction();
		t.commit();

		t.close();

		verify(conn, never()).rollback();
	}

	@Test
	public void testNestedTransactionsOnlySetAutoCommitOnce() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn,
				mock(NodeStoreSQLDialect.class));

		store.beginTransaction();

		// Make sure setAutoCommit was called
		verify(conn).setAutoCommit(false);

		store.beginTransaction();
		store.beginTransaction();

		// Make sure setAutoCommit was still only called once
		verify(conn).setAutoCommit(false);
	}

	@Test
	public void testNestedTransactionsOnlyCallCommitOnOuterTransaction()
			throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn,
				mock(NodeStoreSQLDialect.class));

		InOrder inOrder = inOrder(conn);

		NodeStore.Transaction t1 = store.beginTransaction();
		NodeStore.Transaction t2 = store.beginTransaction();
		NodeStore.Transaction t3 = store.beginTransaction();

		t3.commit();
		verify(conn, never()).commit(); // Make sure that commit isn't called
										// until the outer transaction is
										// committed

		t2.commit();
		verify(conn, never()).commit(); // Make sure that commit isn't called
										// until the outer transaction is
										// committed

		t1.commit();

		inOrder.verify(conn).commit(); // Make sure that commit was called
		inOrder.verify(conn).setAutoCommit(true);
	}

	@Test(expected = IllegalStateException.class)
	public void testNestedTransactionsWithRollbackInMiddle() throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn,
				mock(NodeStoreSQLDialect.class));

		NodeStore.Transaction t1 = store.beginTransaction();
		NodeStore.Transaction t2 = store.beginTransaction();
		NodeStore.Transaction t3 = store.beginTransaction();

		t3.commit();
		t2.close();
		t1.commit();
	}

	@Test(expected = IllegalStateException.class)
	public void testNestedTransactionsWithOutOfOrderCommitsThrowsException()
			throws Exception {
		Connection conn = Mockito.mock(Connection.class);
		JDBCNodeStore store = new JDBCNodeStore(conn,
				mock(NodeStoreSQLDialect.class));

		NodeStore.Transaction t1 = store.beginTransaction();
		NodeStore.Transaction t2 = store.beginTransaction();
		NodeStore.Transaction t3 = store.beginTransaction();

		t3.commit();
		t1.commit(); // t1 must not be committed before t2
		t2.commit();
	}

	@Test
	public void testSelectNodeThreads() throws Exception {
		dbTester.loadData("node_1");
		assertEquals(5, store.getNodeThreads(TEST_SERVER1_NODE1_ID, null, 10)
				.size());
	}

	@Test
	public void testCountNodeThreads() throws Exception {
		dbTester.loadData("node_1");
		assertEquals(5, store.countNodeThreads(TEST_SERVER1_NODE1_ID));
	}

	@Test
	public void testNoNodeOwnersReturnsEmptyList() throws Exception {
		dbTester.loadData("node_1");
		assertEquals(0, store.getNodeOwners(UNKNOWN_NODE).size());
	}

	@Test
	public void testNodeOwnersReturnsExpectedList() throws Exception {
		dbTester.loadData("node_1");
		
		store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID, Subscriptions.subscribed));
		store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER2_JID, Affiliations.owner);
		
		assertEquals(2, store.getNodeOwners(TEST_SERVER1_NODE1_ID).size());
		assertEquals(TEST_SERVER1_USER1_JID, store.getNodeOwners(TEST_SERVER1_NODE1_ID).get(0));
		assertEquals(TEST_SERVER1_USER2_JID, store.getNodeOwners(TEST_SERVER1_NODE1_ID).get(1));
	}
	
	@Test
	public void notPreviousRatingByUserReturnsFalse() throws Exception {

		String node = "/users/romeo@capulet.lit/posts";
		store.addRemoteNode(node);
		
		String content = "" +
			    "<entry xmlns='http://www.w3.org/2005/Atom' xmlns:thr='http://purl.org/syndication/thread/1.0' xmlns:activity='http://activitystrea.ms/spec/1.0/'>" +
				    "<id>tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,6</id>" +
		            "<title>Post title</title>" +
		            "<published>2014-01-01T00:00:00.000Z</published>" +
		            "<updated>2014-01-01T00:00:00.000Z</updated>" +
		            "<author>" +
		                  "<name>romeo@capulet.lit</name>" +
		                  "<uri>acct:romeo@capulet.lit</uri>" +
		                  "<jid xmlns='http://buddycloud.com/atom-elements-0'>romeo@capulet.lit</jid>" +
		            "</author>" +
		               "<content type='text'>rating:5.0</content>" +
		               "<activity:verb>rated</activity:verb>" +
		               "<activity:object>" +
		                  "<activity:object-type>comment</activity:object-type>" +
		               "</activity:object>" +
		               "<thr:in-reply-to ref='tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,5' />" +
		               "<activity:target>" +
		                   "<id>tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,5</id>" +
		               "</activity:target>" +
		               "<review:rating>5.0</review:rating>" +
		           "</entry>";
				String alternativeContent = "" +
		            "<entry xmlns='http://www.w3.org/2005/Atom' xmlns:thr='http://purl.org/syndication/thread/1.0' xmlns:activity='http://activitystrea.ms/spec/1.0/'>" +
					    "<id>tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,7</id>" +
			            "<title>Post title</title>" +
			            "<published>2014-01-01T00:00:00.000Z</published>" +
			               "<content type='text'>rating:5.0</content>" +
			               "<activity:verb>rated</activity:verb>" +
			               "<activity:object>" +
			                  "<activity:object-type>comment</activity:object-type>" +
			               "</activity:object>" +
			               "<thr:in-reply-to ref='tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,5' />" +
			               "<activity:target>" +
			                   "<id>tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,5</id>" +
			               "</activity:target>" +
			               "<review:rating>5.0</review:rating>" +
			           "</entry>";
		
		NodeItem item1 = new NodeItemImpl(node, "6", new Date(), content);
		store.addNodeItem(item1);
		NodeItem item2 = new NodeItemImpl(node, "7", new Date(), alternativeContent);
		store.addNodeItem(item2);
		
		Assert.assertFalse(
			store.userHasRatedPost(node, new JID("romeo@capulet.lit"), new GlobalItemIDImpl(new JID("channels.capulet.lit"), node, "6"))
	    );
	}
	
	@Test
	public void previousRatingByUserReturnsTrue() throws Exception {

		String node = "/users/romeo@capulet.lit/posts";
		store.addRemoteNode(node);
		
		String content = "" +
			    "<entry xmlns='http://www.w3.org/2005/Atom' xmlns:thr='http://purl.org/syndication/thread/1.0' xmlns:activity='http://activitystrea.ms/spec/1.0/'>" +
				    "<id>tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,6</id>" +
		            "<title>Post title</title>" +
		            "<published>2014-01-01T00:00:00.000Z</published>" +
		            "<updated>2014-01-01T00:00:00.000Z</updated>" +
		            "<author>" +
		                  "<name>romeo@capulet.lit</name>" +
		                  "<uri>acct:romeo@capulet.lit</uri>" +
		                  "<jid xmlns='http://buddycloud.com/atom-elements-0'>romeo@capulet.lit</jid>" +
		            "</author>" +
		               "<content type='text'>rating:5.0</content>" +
		               "<activity:verb>rated</activity:verb>" +
		               "<activity:object>" +
		                  "<activity:object-type>comment</activity:object-type>" +
		               "</activity:object>" +
		               "<thr:in-reply-to ref='tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,5' />" +
		               "<activity:target>" +
		                   "<id>tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,5</id>" +
		               "</activity:target>" +
		               "<review:rating>5.0</review:rating>" +
		           "</entry>";
				String alternativeContent = "" +
		            "<entry xmlns='http://www.w3.org/2005/Atom' xmlns:thr='http://purl.org/syndication/thread/1.0' xmlns:activity='http://activitystrea.ms/spec/1.0/'>" +
					    "<id>tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,7</id>" +
			            "<title>Post title</title>" +
			            "<published>2014-01-01T00:00:00.000Z</published>" +
			               "<content type='text'>rating:5.0</content>" +
			               "<activity:verb>rated</activity:verb>" +
			               "<activity:object>" +
			                  "<activity:object-type>comment</activity:object-type>" +
			               "</activity:object>" +
			               "<thr:in-reply-to ref='tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,5' />" +
			               "<activity:target>" +
			                   "<id>tag:channels.capulet.lit,/users/romeo@capulet.lit/posts,5</id>" +
			               "</activity:target>" +
			               "<review:rating>5.0</review:rating>" +
			           "</entry>";
		
		NodeItem item1 = new NodeItemImpl(node, "6", new Date(), content);
		store.addNodeItem(item1);
		NodeItem item2 = new NodeItemImpl(node, "7", new Date(), alternativeContent);
		store.addNodeItem(item2);
		
		Assert.assertTrue(
			store.userHasRatedPost(node, new JID("romeo@capulet.lit"), new GlobalItemIDImpl(new JID("channels.capulet.lit"), node, "5"))
	    );
	}

}