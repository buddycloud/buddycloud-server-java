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

public class JDBCNodeStoreRecentItemsTest extends JDBCNodeStoreAbstract {

    public JDBCNodeStoreRecentItemsTest() throws SQLException, IOException, ClassNotFoundException {
        dbTester = new DatabaseTester();
        IQTestHandler.readConf();
    }

    @Test
    public void testGetRecentItems() throws Exception {

        Date since = new Date();
        dbTester.loadData("node_1");
        store.addRemoteNode(TEST_SERVER1_NODE2_ID);
        store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, null));

        NodeItem nodeItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "123", new Date(), "payload");
        store.addNodeItem(nodeItem1);
        Thread.sleep(1);
        NodeItem nodeItem2 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "123", new Date(), "payload2");
        store.addNodeItem(nodeItem2);

        Thread.sleep(20);

        CloseableIterator<NodeItem> items = store.getRecentItems(TEST_SERVER1_USER1_JID, since, -1, -1, null, null, false);

        // 2 -> 1 on purpose results are most recent first!
        assertSameNodeItem(items.next(), nodeItem2);
        assertSameNodeItem(items.next(), nodeItem1);
        assertEquals(false, items.hasNext());
    }
    
    @Test
    public void testGetRecentItemsNoSubscriptions() throws Exception {
        CloseableIterator<NodeItem> items = store.getRecentItems(
            TEST_SERVER1_USER1_JID, new Date(), -1, -1, null, null, false);
        assertEquals(false, items.hasNext());
    }
    
    @Test
    public void testGetRecentItemsFromEphemeralNode() throws Exception {

        Date since = new Date();
        dbTester.loadData("node_1");
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, "buddycloud#ephemeral", "true");

        NodeItem nodeItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "123", new Date(), "payload");
        store.addNodeItem(nodeItem1);

        Thread.sleep(20);

        CloseableIterator<NodeItem> items = store.getRecentItems(TEST_SERVER1_USER1_JID, since, -1, -1, null, null, false);

        // 2 -> 1 on purpose results are most recent first!
        assertSameNodeItem(items.next(), nodeItem1);
        assertEquals(false, items.hasNext());
    }

    @Test
    public void testGetRecentItemsCanBePaged() throws Exception {

        Date since = new Date(0);
        dbTester.loadData("node_1");
        store.addRemoteNode(TEST_SERVER1_NODE2_ID);
        store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, null));

        long now = System.currentTimeMillis();

        NodeItem nodeItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "123", new Date(now - 400), "<entry><id>123</id></entry>");
        store.addNodeItem(nodeItem1);

        NodeItem nodeItem2 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "123", new Date(now - 400), "<entry><id>123</id></entry>");
        store.addNodeItem(nodeItem2);

        NodeItem nodeItem3 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "124", new Date(now - 300), "<entry><id>124</id></entry>");
        store.addNodeItem(nodeItem3);

        NodeItem nodeItem4 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "124", new Date(now - 200), "<entry><id>124</id></entry>");
        store.addNodeItem(nodeItem4);

        CloseableIterator<NodeItem> items =
                store.getRecentItems(TEST_SERVER1_USER1_JID, since, -1, 2, new GlobalItemIDImpl(TEST_SERVER1_CHANNELS_JID, TEST_SERVER1_NODE1_ID, "124"),
                        null, false);

        assertSameNodeItem(items.next(), nodeItem3);
        assertSameNodeItem(items.next(), nodeItem1);
        assertFalse(items.hasNext());
    }

    @Test
    public void testGetRecentItemsWithNoResultsPerNodeRequestedReturnsExpectedCount() throws Exception {
        Date since = new Date(0);
        dbTester.loadData("node_1");
        store.addRemoteNode(TEST_SERVER1_NODE2_ID);
        store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, null));

        NodeItem nodeItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "1", new Date(), "payload");
        NodeItem nodeItem2 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "2", new Date(), "payload2");
        store.addNodeItem(nodeItem1);
        store.addNodeItem(nodeItem2);

        CloseableIterator<NodeItem> items = store.getRecentItems(TEST_SERVER1_USER1_JID, since, 0, -1, null, null, false);

        int count = 0;
        while (items.hasNext()) {
            items.next();
            ++count;
        }
        assertEquals(0, count);
    }

    @Test
    public void testGetRecentItemCountWithNoResultsPerNodeRequestedReturnsExpectedCount() throws Exception {
        Date since = new Date();
        dbTester.loadData("node_1");
        store.addRemoteNode(TEST_SERVER1_NODE2_ID);
        store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, null));

        store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE1_ID, "123", new Date(), "payload"));
        store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE2_ID, "123", new Date(), "payload2"));

        int count = store.getCountRecentItems(TEST_SERVER1_USER1_JID, since, 0, null, false);
        assertEquals(0, count);
    }

    @Test
    public void testCanPageGetRecentItemsUsingResultSetManagement() throws Exception {
        dbTester.loadData("node_1");

        Date since = new Date();

        Thread.sleep(10);

        store.addRemoteNode(TEST_SERVER1_NODE2_ID);
        store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, null));

        for (int i = 1; i < 20; i++) {
            Thread.sleep(10);
            store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE1_ID, String.valueOf(i), new Date(), "payload" + String.valueOf(i)));
        }

        GlobalItemID itemID = new GlobalItemIDImpl(TEST_SERVER1_CHANNELS_JID, TEST_SERVER1_NODE1_ID, "15");

        CloseableIterator<NodeItem> items = store.getRecentItems(TEST_SERVER1_USER1_JID, since, -1, 10, itemID, null, false);

        int count = 0;
        int i = 14;

        while (items.hasNext()) {
            assertSameNodeItem(items.next(), new NodeItemImpl(TEST_SERVER1_NODE1_ID, String.valueOf(i), new Date(), "payload" + String.valueOf(i)));
            --i;
            ++count;
        }
        assertEquals(10, count);
    }

    @Test
    public void testGetRecentItemCount() throws Exception {

        Date since = new Date();
        dbTester.loadData("node_1");
        store.addRemoteNode(TEST_SERVER1_NODE2_ID);
        store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, null));
        Thread.sleep(1);
        store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE1_ID, "123", new Date(), "payload"));
        store.addNodeItem(new NodeItemImpl(TEST_SERVER1_NODE2_ID, "123", new Date(), "payload2"));

        int count = store.getCountRecentItems(TEST_SERVER1_USER1_JID, since, -1, null, false);
        assertEquals(2, count);
    }

    @Test
    public void settingParentOnlyOnlyReturnsThreadParentsForGetRecentItems() throws Exception {

        Date since = new Date();
        dbTester.loadData("node_1");
        store.addRemoteNode(TEST_SERVER1_NODE2_ID);
        store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, null));

        NodeItem nodeItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "12-node1", new Date(), "payload", null, new Date());
        store.addNodeItem(nodeItem1);
        Thread.sleep(1);
        NodeItem nodeItem2 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "123-node2", new Date(), "payload2", "123", new Date());
        store.addNodeItem(nodeItem2);
        Thread.sleep(1);
        NodeItem nodeItem3 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "124-node1", new Date(), "payload3", null, new Date());
        store.addNodeItem(nodeItem3);
        Thread.sleep(20);

        CloseableIterator<NodeItem> items = store.getRecentItems(TEST_SERVER1_USER1_JID, since, -1, -1, null, null, true);

        // 2 -> 1 on purpose results are most recent first!
        assertSameNodeItem(items.next(), nodeItem3);
        assertSameNodeItem(items.next(), nodeItem1);
        assertEquals(false, items.hasNext());

    }

    @Test
    public void settingParentOnlyReturnsThreadParentsForGetRecentItemsCount() throws Exception {
        Date since = new Date();
        since.setTime(0);
        dbTester.loadData("node_1");
        store.addRemoteNode(TEST_SERVER1_NODE2_ID);
        store.purgeNodeItems(TEST_SERVER1_NODE1_ID);
        store.addUserSubscription(new NodeSubscriptionImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, null));

        NodeItem nodeItem1 = new NodeItemImpl(TEST_SERVER1_NODE1_ID, "12-node1", new Date(), "payload", null, new Date());
        store.addNodeItem(nodeItem1);
        Thread.sleep(1);
        NodeItem nodeItem2 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "123-node2", new Date(), "payload2", "123", new Date());
        store.addNodeItem(nodeItem2);
        Thread.sleep(1);
        NodeItem nodeItem3 = new NodeItemImpl(TEST_SERVER1_NODE2_ID, "124-node1", new Date(), "payload3", null, new Date());
        store.addNodeItem(nodeItem3);
        Thread.sleep(20);

        int itemCount = store.getCountRecentItems(TEST_SERVER1_USER1_JID, since, -1, null, true);

        assertEquals(2, itemCount);
    }
}
