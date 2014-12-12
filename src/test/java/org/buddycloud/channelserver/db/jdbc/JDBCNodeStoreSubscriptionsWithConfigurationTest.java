package org.buddycloud.channelserver.db.jdbc;

import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import junit.framework.Assert;

import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.model.NodeMembershipWithConfiguration;
import org.junit.Test;
import org.xmpp.resultsetmanagement.ResultSet;

public class JDBCNodeStoreSubscriptionsWithConfigurationTest extends JDBCNodeStoreAbstract {

  public JDBCNodeStoreSubscriptionsWithConfigurationTest() throws SQLException, IOException,
      ClassNotFoundException {
    dbTester = new DatabaseTester();
    IQTestHandler.readConf();
  }

  @Test
  public void testEmptySubscriptions() throws Exception {
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, new LinkedList<String>(),
            new HashMap<String, String>());
    Assert.assertEquals(0, memberships.size());
  }

  @Test
  public void testSingleSubscriptionWithNoFilter() throws Exception {
    dbTester.loadData("node_1");
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, new LinkedList<String>(),
            new HashMap<String, String>());
    
    Assert.assertEquals(1, memberships.size());
    NodeMembershipWithConfiguration membership = memberships.get(0);
    Assert.assertEquals("users/node1@server1/posts", membership.getMembership().getNodeId());
    Assert.assertTrue(membership.getConfiguration().containsKey("config1"));
    Assert.assertTrue(membership.getConfiguration().containsKey("config2"));
  }
  
  @Test
  public void testSingleSubscriptionWithConfigurationFilter() throws Exception {
    dbTester.loadData("node_1");
    
    LinkedList<String> configFilter = new LinkedList<String>();
    configFilter.add("config1");
    
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, configFilter,
            new HashMap<String, String>());
    
    Assert.assertEquals(1, memberships.size());
    NodeMembershipWithConfiguration membership = memberships.get(0);
    Assert.assertEquals("users/node1@server1/posts", membership.getMembership().getNodeId());
    Assert.assertTrue(membership.getConfiguration().containsKey("config1"));
    Assert.assertFalse(membership.getConfiguration().containsKey("config2"));
  }
  
  @Test
  public void testSingleSubscriptionWithSubscriptionFilter() throws Exception {
    dbTester.loadData("node_1");
    
    Map<String, String> subscriptionFilter = new HashMap<String, String>();
    subscriptionFilter.put("config1", "anyValue");
    
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, new LinkedList<String>(),
            subscriptionFilter);
    
    Assert.assertEquals(0, memberships.size());
  }
  
  @Test
  public void testSingleSubscriptionWithMatchingSubscriptionFilter() throws Exception {
    dbTester.loadData("node_1");
    
    Map<String, String> subscriptionFilter = new HashMap<String, String>();
    subscriptionFilter.put("config1", "Value of config1");
    
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, new LinkedList<String>(),
            subscriptionFilter);
    
    Assert.assertEquals(1, memberships.size());
    NodeMembershipWithConfiguration membership = memberships.get(0);
    Assert.assertEquals("users/node1@server1/posts", membership.getMembership().getNodeId());
    Assert.assertTrue(membership.getConfiguration().containsKey("config1"));
    Assert.assertTrue(membership.getConfiguration().containsKey("config2"));
  }
  
  @Test
  public void testMultiSubscriptionWithNoFilter() throws Exception {
    dbTester.loadData("node_1");
    dbTester.loadData("node_2");
    
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, new LinkedList<String>(),
            new HashMap<String, String>());
    
    Assert.assertEquals(2, memberships.size());
    
    NodeMembershipWithConfiguration membership1 = memberships.get(0);
    Assert.assertEquals("users/node2@server1/posts", membership1.getMembership().getNodeId());
    Assert.assertTrue(membership1.getConfiguration().containsKey("config1"));
    Assert.assertTrue(membership1.getConfiguration().containsKey("config2"));
    
    NodeMembershipWithConfiguration membership2 = memberships.get(1);
    Assert.assertEquals("users/node1@server1/posts", membership2.getMembership().getNodeId());
    Assert.assertTrue(membership2.getConfiguration().containsKey("config1"));
    Assert.assertTrue(membership2.getConfiguration().containsKey("config2"));
  }
  
  @Test
  public void testMultiSubscriptionWithConfigurationFilter() throws Exception {
    dbTester.loadData("node_1");
    dbTester.loadData("node_2");
    
    LinkedList<String> configFilter = new LinkedList<String>();
    configFilter.add("config1");
    
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, configFilter,
            new HashMap<String, String>());
    
    Assert.assertEquals(2, memberships.size());
    
    NodeMembershipWithConfiguration membership1 = memberships.get(0);
    Assert.assertEquals("users/node2@server1/posts", membership1.getMembership().getNodeId());
    Assert.assertTrue(membership1.getConfiguration().containsKey("config1"));
    Assert.assertFalse(membership1.getConfiguration().containsKey("config2"));
    
    NodeMembershipWithConfiguration membership2 = memberships.get(1);
    Assert.assertEquals("users/node1@server1/posts", membership2.getMembership().getNodeId());
    Assert.assertTrue(membership2.getConfiguration().containsKey("config1"));
    Assert.assertFalse(membership2.getConfiguration().containsKey("config2"));
  }
  
  @Test
  public void testMultiSubscriptionWithSubscriptionFilter() throws Exception {
    dbTester.loadData("node_1");
    dbTester.loadData("node_2");
    
    Map<String, String> subscriptionFilter = new HashMap<String, String>();
    subscriptionFilter.put("config1", "anyValue");
    
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, new LinkedList<String>(),
            subscriptionFilter);
    
    Assert.assertEquals(0, memberships.size());
  }
  
  @Test
  public void testMultiSubscriptionWithMatchingSubscriptionFilter() throws Exception {
    dbTester.loadData("node_1");
    dbTester.loadData("node_2");
    
    Map<String, String> subscriptionFilter = new HashMap<String, String>();
    subscriptionFilter.put("config1", "Value of config1");
    
    store.setNodeConfValue("users/node1@server1/posts", "config1", "Updated value");
    
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, new LinkedList<String>(),
            subscriptionFilter);
    
    Assert.assertEquals(1, memberships.size());
    
    NodeMembershipWithConfiguration membership1 = memberships.get(0);
    Assert.assertEquals("users/node2@server1/posts", membership1.getMembership().getNodeId());
    Assert.assertTrue(membership1.getConfiguration().containsKey("config1"));
    Assert.assertTrue(membership1.getConfiguration().containsKey("config2"));
  }
  
  @Test
  public void testMultiSubscriptionWithMatchingSubscriptionAndConfigurationFilter() throws Exception {
    dbTester.loadData("node_1");
    dbTester.loadData("node_2");
    
    Map<String, String> subscriptionFilter = new HashMap<String, String>();
    subscriptionFilter.put("config1", "Value of config1");
    
    LinkedList<String> configFilter = new LinkedList<String>();
    configFilter.add("config1");
    
    store.setNodeConfValue("users/node1@server1/posts", "config1", "Updated value");
    
    ResultSet<NodeMembershipWithConfiguration> memberships =
        store.getUserMembershipsWithConfiguration(TEST_SERVER1_USER1_JID, configFilter, subscriptionFilter);
    
    Assert.assertEquals(1, memberships.size());
    
    NodeMembershipWithConfiguration membership1 = memberships.get(0);
    Assert.assertEquals("users/node2@server1/posts", membership1.getMembership().getNodeId());
    Assert.assertTrue(membership1.getConfiguration().containsKey("config1"));
    Assert.assertFalse(membership1.getConfiguration().containsKey("config2"));
  }

}
