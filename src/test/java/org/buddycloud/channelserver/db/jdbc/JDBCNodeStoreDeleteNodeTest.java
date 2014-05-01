package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashMap;

import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;

public class JDBCNodeStoreDeleteNodeTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreDeleteNodeTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
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
}