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
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.ResultSet;

public class JDBCNodeStoreAffiliationsTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreAffiliationsTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
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
}