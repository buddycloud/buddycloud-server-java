package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.*;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Collections;
import java.util.HashMap;

import org.buddycloud.channelserver.db.NodeStoreException;
import org.buddycloud.channelserver.node.NodeRef;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.xmpp.packet.JID;


public class JDBCNodeStoreTest {

	private static final String TEST_NODEID_1 = "/users/test@test.com/posts";
	private static final String TEST_NODEID_2 = "/users/testing@testing.com/posts";
	
	private static final JID TEST_JID_1 = new JID("test@test.com");
	private static final JID TEST_JID_2 = new JID("testing@testing.com");
	
	DatabaseTester dbTester;
	Connection conn;
	
	JDBCNodeStore store;
	
	public JDBCNodeStoreTest() throws SQLException, IOException, ClassNotFoundException {
		dbTester = new DatabaseTester();
	}
	
	@Before
	public void setUp() throws Exception {
		dbTester.initialise();
		
		store = new JDBCNodeStore(dbTester.getConnection());
	}

	@After
	public void tearDown() throws Exception {
		dbTester.close();
	}

	@Test
	public void testCreateNodeNoConfig() throws Exception {
		store.createNode(TEST_JID_1, TEST_NODEID_1, new HashMap<String,String>());
		
		HashMap<String,Object> expected = new HashMap<String,Object>();
		
		expected.put("node", TEST_NODEID_1);
		
		dbTester.assertions().assertTableContains("nodes", expected);
		dbTester.assertions().assertTableContains("node_config", expected, 0);

		expected = new HashMap<String,Object>();
		expected.put("node", TEST_NODEID_1);
		expected.put("user", TEST_JID_1.toString());
		expected.put("affiliation", "owner");
		dbTester.assertions().assertTableContains("affiliations", expected);
	}

	@Test
	public void testCreateNodeWithConfig() throws Exception {
		HashMap<String,String> config = new HashMap<String,String>();
		config.put("CONFIG1", "Value of CONFIG1");
		config.put("CONFIG2", "Value of CONFIG2");
		
		store.createNode(TEST_JID_1, TEST_NODEID_1, new HashMap<String,String>());
		
		HashMap<String,Object> expected = new HashMap<String,Object>();
		expected.put("node", TEST_NODEID_1);
		dbTester.assertions().assertTableContains("nodes", expected);

		expected = new HashMap<String,Object>();
		expected.put("node", TEST_NODEID_1);
		expected.put("user", TEST_JID_1.toString());
		expected.put("affiliation", "owner");
		dbTester.assertions().assertTableContains("affiliations", expected);

		expected = new HashMap<String,Object>();
		expected.put("node", TEST_NODEID_1);
		expected.put("key", "CONFIG1");
		expected.put("value", "Value of CONFIG1");
		dbTester.assertions().assertTableContains("node_config", expected);

		expected = new HashMap<String,Object>();
		expected.put("node", TEST_NODEID_1);
		expected.put("key", "CONFIG2");
		expected.put("value", "Value of CONFIG2");
		dbTester.assertions().assertTableContains("node_config", expected);
	}
	
	@Test(expected=NodeStoreException.class)
	public void testCreateDuplicateNodeThrowsException() throws Exception {
		store.createNode(TEST_JID_1, TEST_NODEID_1, new HashMap<String,String>());
		store.createNode(TEST_JID_2, TEST_NODEID_1, new HashMap<String,String>());
	}
	
	@SuppressWarnings("serial")
	@Test
	public void testSetNodeConfExistingConf() throws Exception {
		dbTester.loadData("single_node");
		
		store.setNodeConf("node1", "config1", "updated config1");

		dbTester.assertions().assertTableContains("node_config", new HashMap<String,Object>() {{
			put("node", "node1");
			put("key", "config1");
			put("value", "updated config1");
		}});

		// Make sure the old config isn't there
		dbTester.assertions().assertTableContains("node_config", new HashMap<String,Object>() {{
			put("node", "node1");
			put("key", "config1");
			put("value", "Value of config1");
		}}, 0);
	}
	
	@SuppressWarnings("serial")
	@Test
	public void testSetNodeConfNewConf() throws Exception {
		dbTester.loadData("single_node");
		
		store.setNodeConf("node1", "config3", "Value of config3");

		dbTester.assertions().assertTableContains("node_config", new HashMap<String,Object>() {{
			put("node", "node1");
			put("key", "config3");
			put("value", "Value of config3");
		}});
	}
	
	@SuppressWarnings("serial")
	@Test
	public void testSetUserAffiliationNewAffiliation() throws Exception {
		dbTester.loadData("single_node");

		store.setUserAffiliation("node1", new JID("user2@example.com"), Affiliations.member);
		
		dbTester.assertions().assertTableContains("affiliations", new HashMap<String,Object>() {{
			put("node", "node1");
			put("user", "user2@example.com");
			put("affiliation", Affiliations.member.toString());
		}});
	}
	
	@SuppressWarnings("serial")
	@Test
	public void testSetUserAffiliationUpdateAffiliation() throws Exception {
		dbTester.loadData("single_node");

		store.setUserAffiliation("node1", new JID("user1@example.com"), Affiliations.member);
		
		dbTester.assertions().assertTableContains("affiliations", new HashMap<String,Object>() {{
			put("node", "node1");
			put("user", "user1@example.com");
			put("affiliation", Affiliations.member.toString());
		}});

		dbTester.assertions().assertTableContains("affiliations", new HashMap<String,Object>() {{
			put("node", "node1");
			put("user", "user1@example.com");
			put("affiliation", Affiliations.owner.toString());
		}}, 0);
	}
}
