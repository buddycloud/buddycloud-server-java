package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashSet;

import junit.framework.Assert;

import org.apache.commons.collections.CollectionUtils;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.resultsetmanagement.ResultSet;

public class JDBCNodeStoreNodeMembersTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreNodeMembersTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
	}

	@Test
	public void getNodeMemberships() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeMembership> result = store
				.getNodeMemberships(TEST_SERVER1_NODE1_ID);

		HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
			{
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER3_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed, Affiliations.member, null));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_OUTCAST_JID, Subscriptions.subscribed, Affiliations.outcast, null,
						new Date()));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed, Affiliations.publisher, null,
						new Date()));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER2_JID, Subscriptions.subscribed, Affiliations.publisher, null,
						new Date()));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, null,
						new Date()));
			}
		};

		assertEquals("Incorrect number of node memberships returned",
				expected.size(), result.size());

		assertTrue("Incorrect node memberships returned",
				CollectionUtils.isEqualCollection(expected, result));
	}

	@Test
	public void getNodeMembershipsUsesBareJID() throws Exception {
		dbTester.loadData("node_1");
		dbTester.loadData("node_2");

		ResultSet<NodeMembership> result = store
				.getNodeMemberships(TEST_SERVER1_NODE1_ID);

		HashSet<NodeMembership> expected = new HashSet<NodeMembership>() {
			{
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER3_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed, Affiliations.member, null));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_OUTCAST_JID, Subscriptions.subscribed, Affiliations.outcast, null,
						new Date()));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER2_USER1_JID, TEST_SERVER2_CHANNELS_JID,
						Subscriptions.subscribed, Affiliations.publisher, null,
						new Date()));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER2_JID, Subscriptions.subscribed, Affiliations.publisher, null,
						new Date()));
				add(new NodeMembershipImpl(TEST_SERVER1_NODE1_ID,
						TEST_SERVER1_USER1_JID, Subscriptions.subscribed, Affiliations.owner, null,
						new Date()));
			}
		};

		assertEquals("Incorrect number of node memberships returned",
				expected.size(), result.size());
		assertTrue("Incorrect node memberships returned",
				CollectionUtils.isEqualCollection(expected, result));
	}
	@Test
	public void canGetNodeMembershipWhereTheresOnlySubscription() throws Exception {
		dbTester.loadData("node_1");

		store.deleteUserAffiliations(TEST_SERVER1_USER1_JID);
		
		ResultSet<NodeMembership> result = store.getNodeMemberships(TEST_SERVER1_NODE1_ID);

		NodeMembership expected = new NodeMembershipImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER2_USER3_JID,
				TEST_SERVER2_CHANNELS_JID, Subscriptions.subscribed, Affiliations.member, null);

		assertEquals("An unexpected node membership was returned", expected,
				result.get(0));
	}
	
	@Test
	public void querySelectsTheMostRecentUpdatedDate() throws Exception {
		dbTester.loadData("node_1");
		Date originalDate = store.getNodeMemberships(TEST_SERVER1_NODE1_ID).get(0).getLastUpdated();
        
        store.setUserAffiliation(
    		TEST_SERVER1_NODE1_ID,
    		TEST_SERVER1_USER1_JID,
    		Affiliations.owner
        );
		Date newDate = store.getNodeMembership(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID).getLastUpdated();

		assertTrue(newDate.after(originalDate));
	}
	
	@Test
	public void getNodeMembershipReturnsInvitedByDetails() throws Exception {
		store.addRemoteNode(TEST_SERVER1_NODE1_ID);
		NodeSubscription newSubscription = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.invited, TEST_SERVER2_USER2_JID);
		store.addUserSubscription(newSubscription);
		NodeMembership membership = store.getNodeMembership(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);
		Assert.assertEquals(newSubscription.getInvitedBy(), membership.getInvitedBy());
	}
	
	@Test
	public void getNodeMembershipsReturnsInvitedByDetails() throws Exception {
		store.addRemoteNode(TEST_SERVER1_NODE1_ID);
		NodeSubscription newSubscription = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.invited, TEST_SERVER2_USER2_JID);
		store.addUserSubscription(newSubscription);
		NodeMembership membership = store.getNodeMemberships(TEST_SERVER1_NODE1_ID).get(0);
		Assert.assertEquals(newSubscription.getInvitedBy(), membership.getInvitedBy());
	}
	
	@Test
	public void getUserMembershipsReturnsInvitedByDetails() throws Exception {
		store.addRemoteNode(TEST_SERVER1_NODE1_ID);
		NodeSubscription newSubscription = new NodeSubscriptionImpl(TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID, Subscriptions.invited, TEST_SERVER2_USER2_JID);
		store.addUserSubscription(newSubscription);
		NodeMembership membership = store.getUserMemberships(TEST_SERVER1_USER1_JID).get(0);
		Assert.assertEquals(newSubscription.getInvitedBy(), membership.getInvitedBy());
	}
}