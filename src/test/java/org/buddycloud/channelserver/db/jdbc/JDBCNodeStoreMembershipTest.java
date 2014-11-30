package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;

import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;

public class JDBCNodeStoreMembershipTest extends JDBCNodeStoreAbstract {

    public JDBCNodeStoreMembershipTest() throws SQLException, IOException, ClassNotFoundException {
        dbTester = new DatabaseTester();
        IQTestHandler.readConf();
    }

    @Test
    public void canGetNodeMembership() throws Exception {
        dbTester.loadData("node_1");

        NodeMembership result = store.getNodeMembership(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

        NodeMembership expected =
                new NodeMembershipImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, null);

        assertEquals("An unexpected node membership was returned", expected, result);
    }

    @Test
    public void canGetNodeMembershipWhereTheresOnlySubscription() throws Exception {
        dbTester.loadData("node_1");

        store.deleteUserAffiliations(TEST_SERVER1_USER1_JID);

        NodeMembership result = store.getNodeMembership(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

        NodeMembership expected = new NodeMembershipImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.none, null);

        assertEquals("An unexpected node membership was returned", expected, result);
    }

    @Test
    public void canGetNodeMembershipWhereTheresNoMembership() throws Exception {

        NodeMembership result = store.getNodeMembership(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

        NodeMembership expected = new NodeMembershipImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.none, Affiliations.none, null);

        assertEquals("An unexpected node membership was returned", expected, result);
    }

    @Test
    public void querySelectsTheMostRecentUpdatedDate() throws Exception {
        dbTester.loadData("node_1");
        Date originalDate = store.getNodeMembership(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID).getLastUpdated();

        store.setUserAffiliation(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Affiliations.owner);
        Date newDate = store.getNodeMembership(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID).getLastUpdated();

        assertTrue(newDate.after(originalDate));
    }

}
