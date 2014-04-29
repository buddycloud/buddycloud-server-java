package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import junit.framework.Assert;

import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;

public class JDBCNodeStoreNodeExistsTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreNodeExistsTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
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
}