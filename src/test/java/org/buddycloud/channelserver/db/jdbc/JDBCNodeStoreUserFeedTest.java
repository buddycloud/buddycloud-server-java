package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;

import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;

public class JDBCNodeStoreUserFeedTest extends JDBCNodeStoreAbstract {


	public JDBCNodeStoreUserFeedTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
	}

	@Test
	public void getUserFeed() throws Exception {

		dbTester.loadData("node_1");
		
		CloseableIterator<NodeItem> items = store.getUserFeedItems(
				TEST_SERVER1_USER1_JID, new Date(0), -1, null, false);

		// 2 -> 1 on purpose results are most recent first!
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a5",
				new Date(), "payload2"), false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a4",
				new Date(), "payload"), false);
		assertEquals(true, items.hasNext());

		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a3",
				new Date(), "payload"), false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a2",
				new Date(), "payload"), false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a1",
				new Date(), "payload"), false);
		assertEquals(false, items.hasNext());
	}
	
	@Test
	public void specifyingNowAsSinceDateRetunsNoItems() throws Exception {
        dbTester.loadData("node_1");
		
		CloseableIterator<NodeItem> items = store.getUserFeedItems(
				TEST_SERVER1_USER1_JID, new Date(), -1, null, false);
		
		assertEquals(false, items.hasNext());
	}

	@Test
	public void testGetUserFeedCanBePaged() throws Exception {

		Date since = new Date(0);
		dbTester.loadData("node_1");
		
		CloseableIterator<NodeItem> items = store.getUserFeedItems(
				TEST_SERVER1_USER1_JID, new Date(0), 2, null, false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a5",
				new Date(), "payload2"), false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a4",
				new Date(), "payload"), false);
		assertFalse(items.hasNext());
		
		items = store.getUserFeedItems(
				TEST_SERVER1_USER1_JID, new Date(0), 2, new GlobalItemIDImpl(TEST_SERVER1_CHANNELS_JID,
						TEST_SERVER1_NODE1_ID, "a3"), false);

		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a2",
				new Date(), "payload2"), false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a1",
				new Date(), "payload"), false);
		assertFalse(items.hasNext());
	}


	@Test
	public void settingParentOnlyOnlyReturnsThreadParentsForGetUserFeed() throws Exception {

		dbTester.loadData("node_1");
		
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a5-1", new Date(), "payload-a5-1", "a5"));
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a4-1", new Date(), "payload-a4-1", "a4"));
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a4-2", new Date(), "payload-a4-2", "a4"));
		
		CloseableIterator<NodeItem> items = store.getUserFeedItems(
				TEST_SERVER1_USER1_JID, new Date(0), -1, null, true);

		// 2 -> 1 on purpose results are most recent first!
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a5",
				new Date(), "payload2"), false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a4",
				new Date(), "payload"), false);
		assertEquals(true, items.hasNext());

		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a3",
				new Date(), "payload"), false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a2",
				new Date(), "payload"), false);
		assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, "a1",
				new Date(), "payload"), false);
		assertEquals(false, items.hasNext());
	}
		
	@Test
	public void getUserFeedItemCount() throws Exception {

		Date since = new Date();
		dbTester.loadData("node_1");
		store.addRemoteNode(TEST_SERVER1_NODE2_ID);
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed, null));
		Thread.sleep(1);
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE1_ID, "123",
				new Date(), "payload"));
		store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE2_ID, "123",
				new Date(), "payload2"));

		int itemCount = store.getCountUserFeedItems(
				TEST_SERVER1_USER1_JID, since, true);
		assertEquals(2, itemCount);
	}
	
	@Test
	public void getZeroUserFeedCountIfSinceDateIsSetToNow() throws Exception {

		dbTester.loadData("node_1");
		
		int itemCount = store.getCountUserFeedItems(
				TEST_SERVER1_USER1_JID, new Date(), true);
		assertEquals(0, itemCount);
	}

	@Test
	public void settingParentOnlyReturnsThreadParentsForGetUserFeedCount() throws Exception {
		Date since = new Date();
		since.setTime(0);
		dbTester.loadData("node_1");
		store.addRemoteNode(TEST_SERVER1_NODE2_ID);
		store.purgeNodeItems(TEST_SERVER1_NODE1_ID);
		store.addUserSubscription(new NodeSubscriptionImpl(
				TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed, null));

		NodeItem nodeItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "12-node1",
				new Date(), "payload");
		store.addNodeItem(nodeItem1);
		Thread.sleep(1);
		NodeItem nodeItem2 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "123-node2",
				new Date(), "payload2", "123");
		store.addNodeItem(nodeItem2);
		Thread.sleep(1);
		NodeItem nodeItem3 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "124-node1",
				new Date(), "payload3");
		store.addNodeItem(nodeItem3);
		Thread.sleep(20);

		int itemCount = store.getCountUserFeedItems(
				TEST_SERVER1_USER1_JID, since, true);

		assertEquals(2, itemCount);
	}
}