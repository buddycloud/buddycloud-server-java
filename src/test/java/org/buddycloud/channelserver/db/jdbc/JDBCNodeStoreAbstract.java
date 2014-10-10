package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.buddycloud.channelserver.db.jdbc.dialect.Sql92NodeStoreDialect;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.junit.After;
import org.junit.Before;
import org.xmpp.packet.JID;

@SuppressWarnings("serial")
public abstract class JDBCNodeStoreAbstract {

    protected static final String TEST_SERVER1_NODE1_ID = "users/node1@server1/posts";
    protected static final String TEST_SERVER1_NODE2_ID = "users/node2@server1/posts";
    protected static final String TEST_SERVER1_NODE3_ID = "users/node3@server1/posts";

    protected static final String TEST_SERVER2_NODE1_ID = "users/node1@server2/posts";

    protected static final String TEST_SERVER1_NODE1_ITEM1_ID = "a1";
    protected static final String TEST_SERVER1_NODE1_ITEM1_CONTENT = "Test 1";

    protected static final String TEST_SERVER1_NODE1_ITEM2_ID = "a2";
    protected static final String TEST_SERVER1_NODE1_ITEM2_CONTENT = "Test 2";

    protected static final String TEST_SERVER1_NODE1_ITEM3_ID = "a3";
    protected static final String TEST_SERVER1_NODE1_ITEM3_CONTENT = "Test 3";

    protected static final String TEST_SERVER1_NODE1_ITEM4_ID = "a4";
    protected static final String TEST_SERVER1_NODE1_ITEM4_GLOBAL_ID = new GlobalItemIDImpl(
            new JID("server1"), TEST_SERVER1_NODE1_ID, TEST_SERVER1_NODE1_ITEM4_ID).toString();
    protected static final String TEST_SERVER1_NODE1_ITEM4_CONTENT = "Test 4";

    protected static final String TEST_SERVER1_NODE1_ITEM5_ID = "a5";
    protected static final String TEST_SERVER1_NODE1_ITEM5_CONTENT = "Test 5";

    protected static final String TEST_SERVER1_HOSTNAME = "server1";
    protected static final String TEST_SERVER2_HOSTNAME = "server2";

    protected static final String UNKNOWN_NODE = "/user/unknown@example.com/posts";

    protected static final HashMap<String, String> TEST_SERVER1_NODE1_CONF = new HashMap<String, String>() {
        {
            put("config1", "Value of config1");
            put("config2", "Value of config2");
        }
    };

    protected static final String TEST_SERVER1_NODE1_CONFIG1_KEY = "config1";
    protected static final String TEST_SERVER1_NODE1_CONFIG1_VALUE = "Value of config1";

    protected static final JID TEST_SERVER1_CHANNELS_JID = new JID("channels.server1");
    protected static final JID TEST_SERVER2_CHANNELS_JID = new JID("channels.server2");

    protected static final JID TEST_SERVER1_USER1_JID = new JID("user1@server1");
    protected static final JID TEST_SERVER1_USER1_JID_WITH_RESOURCE = new JID("user1@server1/resource");
    protected static final JID TEST_SERVER1_USER2_JID = new JID("user2@server1");
    protected static final JID TEST_SERVER1_USER3_JID = new JID("user3@server1");
    protected static final JID TEST_SERVER1_OUTCAST_JID = new JID("outcast@server1");

    protected static final JID TEST_SERVER2_USER1_JID = new JID("user1@server2");
    protected static final JID TEST_SERVER2_USER2_JID = new JID("user2@server2");
    protected static final JID TEST_SERVER2_USER3_JID = new JID("user3@server2");

    DatabaseTester dbTester;
    Connection conn;

    JDBCNodeStore store;

    public JDBCNodeStoreAbstract() throws SQLException, IOException, ClassNotFoundException {
        dbTester = new DatabaseTester();
        IQTestHandler.readConf();
    }

    @Before
    public void setUp() throws Exception {
        dbTester.initialise();

        store = new JDBCNodeStore(dbTester.getConnection(), new Sql92NodeStoreDialect());
    }

    @After
    public void tearDown() throws Exception {
        dbTester.close();
    }

    protected void assertNodeConfigEquals(final String nodeId, final Map<String, String> config) throws Exception {
        // Check there's the correct number of config entries
        dbTester.assertions().assertTableContains("node_config", new HashMap<String, Object>() {
            {
                put("node", nodeId);
            }
        }, config.size());

        for (final Entry<String, String> entry : config.entrySet()) {
            dbTester.assertions().assertTableContains("node_config", new HashMap<String, Object>() {
                {
                    put("node", nodeId);
                    put("key", entry.getKey());
                    put("value", entry.getValue());
                }
            });
        }
    }

    protected void assertSameNodeItem(NodeItem actual, NodeItem expected, boolean checkPayload) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getNodeId(), actual.getNodeId());
        if (true == checkPayload) {
            assertEquals(expected.getPayload(), actual.getPayload());
        }
        assertEquals(expected.getInReplyTo(), actual.getInReplyTo());
    }

    protected void assertSameNodeItem(NodeItem actual, NodeItem expected) {
        assertSameNodeItem(actual, expected, true);
    }
}
