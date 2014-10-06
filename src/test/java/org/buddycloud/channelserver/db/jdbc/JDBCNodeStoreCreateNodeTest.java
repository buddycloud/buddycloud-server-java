package org.buddycloud.channelserver.db.jdbc;

import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;

import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.junit.Test;

public class JDBCNodeStoreCreateNodeTest extends JDBCNodeStoreAbstract {

    public JDBCNodeStoreCreateNodeTest() throws SQLException, IOException, ClassNotFoundException {
        dbTester = new DatabaseTester();
        IQTestHandler.readConf();
    }


    @Test
    public void testCreateNodeNoConfig() throws Exception {
        store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID, new HashMap<String, String>());

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
        store.createNode(TEST_SERVER1_USER1_JID, TEST_SERVER1_NODE1_ID, new HashMap<String, String>());
        store.createNode(TEST_SERVER1_USER2_JID, TEST_SERVER1_NODE1_ID, new HashMap<String, String>());
    }

    @Test
    public void testCreateNodeUsesBareJidForOwner() throws Exception {
        store.createNode(TEST_SERVER1_USER1_JID_WITH_RESOURCE, TEST_SERVER1_NODE1_ID, new HashMap<String, String>());

        HashMap<String, Object> expected = new HashMap<String, Object>();
        expected.put("node", TEST_SERVER1_NODE1_ID);
        expected.put("user", TEST_SERVER1_USER1_JID.toString());
        expected.put("affiliation", "owner");
        dbTester.assertions().assertTableContains("affiliations", expected);
    }
}
