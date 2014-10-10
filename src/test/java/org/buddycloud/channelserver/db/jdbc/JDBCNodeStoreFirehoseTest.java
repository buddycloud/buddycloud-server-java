package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.sql.SQLException;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
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
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.open.toString());
        CloseableIterator<NodeItem> firehose = store.getFirehose(
                Integer.MAX_VALUE, null, false, TEST_SERVER1_HOSTNAME);
        int itemCount = 0;
        while (firehose.hasNext()) {
            firehose.next();
            itemCount++;
        }
        assertEquals(5, itemCount);
    }
    
    @Test
    public void testNonAdminGetFirehoseAuthorizedNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.authorize.toString());
        CloseableIterator<NodeItem> firehose = store.getFirehose(
                Integer.MAX_VALUE, null, false, TEST_SERVER1_HOSTNAME);
        int itemCount = 0;
        while (firehose.hasNext()) {
            firehose.next();
            itemCount++;
        }
        assertEquals(0, itemCount);
    }
    
    @Test
    public void testAdminGetFirehoseAuthorizedNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.authorize.toString());
        CloseableIterator<NodeItem> firehose = store.getFirehose(
                Integer.MAX_VALUE, null, true, TEST_SERVER1_HOSTNAME);
        int itemCount = 0;
        while (firehose.hasNext()) {
            firehose.next();
            itemCount++;
        }
        assertEquals(5, itemCount);
    }
    
    @Test
    public void testAdminGetFirehoseLocalNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.local.toString());
        CloseableIterator<NodeItem> firehose = store.getFirehose(
                Integer.MAX_VALUE, null, true, TEST_SERVER2_HOSTNAME);
        int itemCount = 0;
        while (firehose.hasNext()) {
            firehose.next();
            itemCount++;
        }
        assertEquals(5, itemCount);
    }
    
    @Test
    public void testNonAdminSameDomainGetFirehoseLocalNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.local.toString());
        CloseableIterator<NodeItem> firehose = store.getFirehose(
                Integer.MAX_VALUE, null, false, TEST_SERVER1_HOSTNAME);
        int itemCount = 0;
        while (firehose.hasNext()) {
            firehose.next();
            itemCount++;
        }
        assertEquals(5, itemCount);
    }
    
    @Test
    public void testNonAdminGetFirehoseLocalNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.local.toString());
        CloseableIterator<NodeItem> firehose = store.getFirehose(
                Integer.MAX_VALUE, null, false, TEST_SERVER2_HOSTNAME);
        int itemCount = 0;
        while (firehose.hasNext()) {
            firehose.next();
            itemCount++;
        }
        assertEquals(0, itemCount);
    }
    
    @Test
    public void testGetFirehoseNoDomainSet() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.open.toString());
        Configuration.getInstance().remove(
                Configuration.CONFIGURATION_SERVER_DOMAIN);
        Configuration.getInstance().remove(
                Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN);
        CloseableIterator<NodeItem> firehose = store.getFirehose(
                Integer.MAX_VALUE, null, false, TEST_SERVER1_HOSTNAME);
        int itemCount = 0;
        while (firehose.hasNext()) {
            firehose.next();
            itemCount++;
        }
        assertEquals(5, itemCount);
    }
    
    @Test
    public void testGetFirehoseRSM() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.open.toString());
        CloseableIterator<NodeItem> firehose = store.getFirehose(
                Integer.MAX_VALUE, TEST_SERVER1_NODE1_ITEM4_GLOBAL_ID, 
                false, TEST_SERVER1_HOSTNAME);
        int itemCount = 0;
        while (firehose.hasNext()) {
            firehose.next();
            itemCount++;
        }
        assertEquals(3, itemCount);
    }
    
    @Test
    public void testCountFirehoseItemsOpenNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.open.toString());
        int firehoseItemCount = store.getFirehoseItemCount(false, 
                TEST_SERVER1_HOSTNAME);
        assertEquals(5, firehoseItemCount);
    }
    
    @Test
    public void testNonAdminCountFirehoseItemsAuthorizeNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.authorize.toString());
        int firehoseItemCount = store.getFirehoseItemCount(false, 
                TEST_SERVER1_HOSTNAME);
        assertEquals(0, firehoseItemCount);
    }
    
    @Test
    public void testAdminCountFirehoseItemsAuthorizeNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.authorize.toString());
        int firehoseItemCount = store.getFirehoseItemCount(true, 
                TEST_SERVER1_HOSTNAME);
        assertEquals(5, firehoseItemCount);
    }
    
    @Test
    public void testAdminCountFirehoseLocalNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.local.toString());
        int firehoseItemCount = store.getFirehoseItemCount(true, TEST_SERVER2_HOSTNAME);
        assertEquals(5, firehoseItemCount);
    }
    
    @Test
    public void testNonAdminSameDomainCountFirehoseLocalNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.local.toString());
        int firehoseItemCount = store.getFirehoseItemCount(false, TEST_SERVER1_HOSTNAME);
        assertEquals(5, firehoseItemCount);
    }
    
    @Test
    public void testNonAdminCounttFirehoseLocalNode() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.local.toString());
        int firehoseItemCount = store.getFirehoseItemCount(false, TEST_SERVER2_HOSTNAME);
        assertEquals(0, firehoseItemCount);
    }
    
    @Test
    public void testCountFirehoseItemsNoDomainSet() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Conf.ACCESS_MODEL, AccessModels.open.toString());
        Configuration.getInstance().remove(
                Configuration.CONFIGURATION_SERVER_DOMAIN);
        Configuration.getInstance().remove(
                Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN);
        int firehoseItemCount = store.getFirehoseItemCount(false, 
                TEST_SERVER1_HOSTNAME);
        assertEquals(5, firehoseItemCount);
    }

}
