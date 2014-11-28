package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashSet;

import org.apache.commons.collections.CollectionUtils;
import org.buddycloud.channelserver.channel.node.configuration.field.Ephemeral;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.resultsetmanagement.ResultSet;

public class JDBCNodeStoreUserMembershipsTest extends JDBCNodeStoreAbstract {

    public JDBCNodeStoreUserMembershipsTest() throws SQLException, IOException, ClassNotFoundException {
        dbTester = new DatabaseTester();
        IQTestHandler.readConf();
    }

    @Test
    public void getUserMemberships() throws Exception {
        dbTester.loadData("node_1");
        dbTester.loadData("node_2");

        ResultSet<NodeMembership> result = store.getUserMemberships(TEST_SERVER1_USER1_JID);

        HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
            {
                add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, null, new Date()));
                add(new NodeMembershipImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.publisher, null,
                        new Date()));
            }
        };

        assertEquals("Incorrect number of user memberships returned", expected.size(), result.size());
        assertTrue("Incorrect user memberships returned", CollectionUtils.isEqualCollection(expected, result));
    }

    @Test
    public void getUserMembershipsUsesBareJID() throws Exception {
        dbTester.loadData("node_1");
        dbTester.loadData("node_2");

        ResultSet<NodeMembership> result = store.getUserMemberships(TEST_SERVER1_USER1_JID_WITH_RESOURCE);

        HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
            {
                add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, null, new Date()));
                add(new NodeMembershipImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.publisher, null,
                        new Date()));
            }
        };

        assertEquals("Incorrect number of user memberships returned", expected.size(), result.size());
        assertTrue("Incorrect user memberships returned", CollectionUtils.isEqualCollection(expected, result));
    }

    @Test
    public void canGetUserMembershipWhereTheresOnlySubscription() throws Exception {
        dbTester.loadData("node_1");

        store.deleteUserAffiliations(TEST_SERVER1_USER1_JID);

        ResultSet<NodeMembership> result = store.getUserMemberships(TEST_SERVER1_USER1_JID);

        NodeMembership expected = new NodeMembershipImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.none, null);

        assertEquals("An unexpected user membership was returned", expected, result.get(0));
    }

    @Test
    public void querySelectsTheMostRecentUpdatedDate() throws Exception {
        dbTester.loadData("node_1");
        Date originalDate = store.getUserMemberships(TEST_SERVER1_USER1_JID).get(0).getLastUpdated();

        store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Affiliations.owner);
        Date newDate = store.getNodeMembership(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID).getLastUpdated();

        assertTrue(newDate.after(originalDate));
    }
    
    
    @Test
    public void settingEphmeralToTrueOnlyReturnsMembershipsToEphemeralNodes() throws Exception {

      // Make a couple of these nodes ephemeral - include both no 'true' and a 'false'
      dbTester.loadData("node_1");
      dbTester.loadData("node_2");
      dbTester.loadData("node_3");
      
      store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Ephemeral.FIELD_NAME, "true");
      store.setNodeConfValue(TEST_SERVER1_NODE2_ID, Ephemeral.FIELD_NAME, "false");
      
      ResultSet<NodeMembership> result = store.getUserMemberships(TEST_SERVER1_USER1_JID, true);

      HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
          {
              add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, null, new Date()));
          }
      };

      assertEquals("Incorrect number of user memberships returned", expected.size(), result.size());
      assertTrue("Incorrect user memberships returned", CollectionUtils.isEqualCollection(expected, result));
    }
    
    @Test
    public void settingEphemeralToFalseOnlyReturnsMembershipsForNonEphemeralNodes() throws Exception {
      // Make a couple of these nodes ephemeral - include both no 'true' and a 'false'
      dbTester.loadData("node_1");
      dbTester.loadData("node_2");
      dbTester.loadData("node_3");

      store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Ephemeral.FIELD_NAME, "true");
      store.setNodeConfValue(TEST_SERVER1_NODE2_ID, Ephemeral.FIELD_NAME, "false");
      
      ResultSet<NodeMembership> result = store.getUserMemberships(TEST_SERVER1_USER1_JID, false);

      HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
          {
              add(new NodeMembershipImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.publisher, null,
                      new Date()));
              add(new NodeMembershipImpl(TEST_SERVER1_NODE3_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, null, new Date()));
          }
      };

      assertEquals("Incorrect number of user memberships returned", expected.size(), result.size());
      assertTrue("Incorrect user memberships returned", CollectionUtils.isEqualCollection(expected, result));
    }
    
    @Test
    public void callingGetUserMembershipsWithoutFlagDefaultsToEphemeralFalse() throws Exception {
      // Make a couple of these nodes ephemeral - include both no 'true' and a 'false'
      dbTester.loadData("node_1");
      dbTester.loadData("node_2");
      dbTester.loadData("node_3");

      store.setNodeConfValue(TEST_SERVER1_NODE1_ID, Ephemeral.FIELD_NAME, "true");
      store.setNodeConfValue(TEST_SERVER1_NODE2_ID, Ephemeral.FIELD_NAME, "false");
      
      ResultSet<NodeMembership> result = store.getUserMemberships(TEST_SERVER1_USER1_JID, false);

      HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
          {
              add(new NodeMembershipImpl(TEST_SERVER1_NODE2_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.publisher, null,
                      new Date()));
              add(new NodeMembershipImpl(TEST_SERVER1_NODE3_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, null, new Date()));
          }
      };

      assertEquals("Incorrect number of user memberships returned", expected.size(), result.size());
      assertTrue("Incorrect user memberships returned", CollectionUtils.isEqualCollection(expected, result));
    }
}
