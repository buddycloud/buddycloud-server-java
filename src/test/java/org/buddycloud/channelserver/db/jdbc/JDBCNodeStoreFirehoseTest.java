package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.sql.SQLException;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.junit.Before;
import org.junit.Test;

public class JDBCNodeStoreFirehoseTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreFirehoseTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
	}
	
	@Before
	public void setup() throws SQLException, IOException {
		dbTester.loadData("node_1");
	}

	@Test
	public void testGetFirehoseOpenNode() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "open");
		CloseableIterator<NodeItem> firehose = store.getFirehose(
				Integer.MAX_VALUE, null, false);
		int itemCount = 0;
		while (firehose.hasNext()) {
			firehose.next();
			itemCount++;
		}
		assertEquals(5, itemCount);
	}
	
	@Test
	public void testNonAdminGetFirehoseAuthorizedNode() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "authorized");
		CloseableIterator<NodeItem> firehose = store.getFirehose(
				Integer.MAX_VALUE, null, false);
		int itemCount = 0;
		while (firehose.hasNext()) {
			firehose.next();
			itemCount++;
		}
		assertEquals(0, itemCount);
	}
	
	@Test
	public void testAdminGetFirehoseAuthorizedNode() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "authorized");
		CloseableIterator<NodeItem> firehose = store.getFirehose(
				Integer.MAX_VALUE, null, true);
		int itemCount = 0;
		while (firehose.hasNext()) {
			firehose.next();
			itemCount++;
		}
		assertEquals(5, itemCount);
	}
	
	@Test
	public void testGetFirehoseNoDomainSet() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "open");
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_SERVER_DOMAIN);
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN);
		CloseableIterator<NodeItem> firehose = store.getFirehose(
				Integer.MAX_VALUE, null, false);
		int itemCount = 0;
		while (firehose.hasNext()) {
			firehose.next();
			itemCount++;
		}
		assertEquals(5, itemCount);
	}
	
	@Test
	public void testGetFirehoseRSM() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "open");
		CloseableIterator<NodeItem> firehose = store.getFirehose(
				Integer.MAX_VALUE, TEST_SERVER1_NODE1_ITEM4_GLOBAL_ID, false);
		int itemCount = 0;
		while (firehose.hasNext()) {
			firehose.next();
			itemCount++;
		}
		assertEquals(3, itemCount);
	}
	
	@Test
	public void testCountFirehoseItemsOpenNode() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "open");
		int firehoseItemCount = store.getFirehoseItemCount(false);
		assertEquals(5, firehoseItemCount);
	}
	
	@Test
	public void testNonAdminCountFirehoseItemsAuthorizeNode() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "authorize");
		int firehoseItemCount = store.getFirehoseItemCount(false);
		assertEquals(0, firehoseItemCount);
	}
	
	@Test
	public void testAdminCountFirehoseItemsAuthorizeNode() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "authorize");
		int firehoseItemCount = store.getFirehoseItemCount(true);
		assertEquals(5, firehoseItemCount);
	}
	
	@Test
	public void testCountFirehoseItemsNoDomainSet() throws Exception {
		store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, "open");
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_SERVER_DOMAIN);
		Configuration.getInstance().remove(
				Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN);
		int firehoseItemCount = store.getFirehoseItemCount(false);
		assertEquals(5, firehoseItemCount);
	}

}