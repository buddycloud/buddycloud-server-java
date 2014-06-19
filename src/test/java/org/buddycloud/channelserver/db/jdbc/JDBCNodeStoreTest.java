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
	public void canUpdateUpdatedDateOfItem() throws Exception {
		dbTester.loadData("node_1");

		NodeItem result = store.getNodeItem(TEST_SERVER1_NODE1_ID,
				TEST_SERVER1_NODE1_ITEM1_ID);
        Date originalDate = result.getUpdated();
        store.updateThreadParent(result.getNodeId(), result.getId());
        result = store.getNodeItem(TEST_SERVER1_NODE1_ID,
				TEST_SERVER1_NODE1_ITEM1_ID);
        assertTrue(result.getUpdated().after(originalDate));
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
	public void testGetIsCachedSubscriptionNodeReturnsFalseWhereThereAreNoSubscriptions()
			throws Exception {
		boolean cached = store.nodeHasSubscriptions(TEST_SERVER1_NODE1_ID);
		assertEquals(false, cached);
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