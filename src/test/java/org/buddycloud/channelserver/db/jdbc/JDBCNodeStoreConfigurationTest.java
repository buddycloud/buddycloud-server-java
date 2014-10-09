package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import junit.framework.Assert;

import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.junit.Test;

public class JDBCNodeStoreConfigurationTest extends JDBCNodeStoreAbstract {

    public JDBCNodeStoreConfigurationTest() throws SQLException, IOException, ClassNotFoundException {
        dbTester = new DatabaseTester();
        IQTestHandler.readConf();
    }

    @SuppressWarnings("serial")
    @Test
    public void testSetNodeConfValueExistingConf() throws Exception {
        dbTester.loadData("node_1");

        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, "config1", "updated config1");

        dbTester.assertions().assertTableContains("node_config", new HashMap<String, Object>() {
            {
                put("node", TEST_SERVER1_NODE1_ID);
                put("key", "config1");
                put("value", "updated config1");
            }
        });

        // Make sure the old config isn't there
        dbTester.assertions().assertTableContains("node_config", new HashMap<String, Object>() {
            {
                put("node", TEST_SERVER1_NODE1_ID);
                put("key", "config1");
                put("value", "Value of config1");
            }
        }, 0);
    }

    @SuppressWarnings("serial")
    @Test
    public void testSetNodeConfValueNewConf() throws Exception {
        dbTester.loadData("node_1");

        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, "config3", "Value of config3");

        dbTester.assertions().assertTableContains("node_config", new HashMap<String, Object>() {
            {
                put("node", TEST_SERVER1_NODE1_ID);
                put("key", "config3");
                put("value", "Value of config3");
            }
        });
    }

    @Test(expected = NullPointerException.class)
    public void testSetNodeConfValueNullNode() throws Exception {
        store.setNodeConfValue(null, "config3", "Value of config3");
    }

    @Test(expected = NullPointerException.class)
    public void testSetNodeConfValueNullKey() throws Exception {
        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, null, "Value of config3");
    }

    @Test
    public void testCanAddSingleNodeConfigurationValue() throws Exception {
        dbTester.loadData("node_1");

        String key = "custom-key";
        String value = "custom-value";

        store.setNodeConfValue(TEST_SERVER1_NODE1_ID, key, value);
        assertEquals(value, store.getNodeConfValue(TEST_SERVER1_NODE1_ID, key));
    }

    @Test
    public void testSetNodeConf() throws Exception {
        dbTester.loadData("node_1");

        @SuppressWarnings("serial")
        HashMap<String, String> updatedConf = new HashMap<String, String>() {
            {
                put("config1", "Value of config 1");
                put("config2", "Value of config 2");
                put("test1", "value1");
                put("test2", "value2");
            }
        };

        store.setNodeConf(TEST_SERVER1_NODE1_ID, updatedConf);

        assertNodeConfigEquals(TEST_SERVER1_NODE1_ID, updatedConf);
    }

    @SuppressWarnings("serial")
    @Test(expected = NullPointerException.class)
    public void testSetNodeConfWithNullKeyThrowsException() throws Exception {
        dbTester.loadData("node_1");

        HashMap<String, String> updatedConf = new HashMap<String, String>() {
            {
                put("config1", "Value of config 1");
                put(null, "value1");
                put("test2", "value2");
            }
        };

        store.setNodeConf(TEST_SERVER1_NODE1_ID, updatedConf);
    }

    @Test
    public void testCanDeleteNodeConfiguration() throws Exception {
        dbTester.loadData("node_1");
        store.deleteNodeConfiguration(TEST_SERVER1_NODE1_ID);
        assertEquals(0, store.getNodeConf(TEST_SERVER1_NODE1_ID).size());
    }

    @SuppressWarnings("serial")
    @Test
    public void testSetNodeConfWithNullKeyIsAtomic() throws Exception {
        dbTester.loadData("node_1");

        HashMap<String, String> updatedConf = new HashMap<String, String>() {
            {
                put("config1", "Value of config 1");
                put(null, "value1");
                put("test2", "value2");
            }
        };

        try {
            store.setNodeConf(TEST_SERVER1_NODE1_ID, updatedConf);
        } catch (NullPointerException e) {
            // ignore
        }

        // Check the config hasn't changed
        assertNodeConfigEquals(TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_CONF);
    }


    @Test
    public void testGetNodeConfValueSuccess() throws Exception {
        dbTester.loadData("node_1");

        assertEquals("Request for config key config1 is incorrect", "Value of config1", store.getNodeConfValue(TEST_SERVER1_NODE1_ID, "config1"));
    }

    @Test
    public void testGetNodeConfValueNonExistantReturnsNull() throws Exception {
        dbTester.loadData("node_1");

        assertNull("Request for non existant config key is incorrect", store.getNodeConfValue(TEST_SERVER1_NODE1_ID, "nonexistant"));
    }

    @Test
    public void testGetNodeConf() throws Exception {
        dbTester.loadData("node_1");

        Map<String, String> result = store.getNodeConf(TEST_SERVER1_NODE1_ID);

        assertEquals("Returned config if incorrect size", TEST_SERVER1_NODE1_CONF.size(), result.size());

        for (Entry<String, String> entry : TEST_SERVER1_NODE1_CONF.entrySet()) {
            assertEquals("Result has incorrect entry for " + entry.getKey(), entry.getValue(), result.get(entry.getKey()));
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
}
