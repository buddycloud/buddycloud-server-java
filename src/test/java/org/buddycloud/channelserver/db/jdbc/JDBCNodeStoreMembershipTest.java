package org.buddycloud.channelserver.db.jdbc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;

import org.apache.commons.collections.CollectionUtils;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.junit.Test;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.ResultSet;

public class JDBCNodeStoreMembershipTest extends JDBCNodeStoreAbstract {

	public JDBCNodeStoreMembershipTest() throws SQLException, IOException,
			ClassNotFoundException {
		dbTester = new DatabaseTester();
		IQTestHandler.readConf();
	}

	@Test
	public void canGetNodeMembership() throws Exception {
		dbTester.loadData("node_1");

		NodeMembership result = store.getNodeMembership(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

		NodeMembership expected = new NodeMembershipImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed, Affiliations.owner);

		assertEquals("An unexpected node membership was returned", expected,
				result);
	}
	
	@Test
	public void canGetNodeMembershipWhereTheresOnlySubscription() throws Exception {
		dbTester.loadData("node_1");

		store.deleteUserAffiliations(TEST_SERVER1_USER1_JID);
		
		NodeMembership result = store.getNodeMembership(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

		NodeMembership expected = new NodeMembershipImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.subscribed, Affiliations.none);

		assertEquals("An unexpected node membership was returned", expected,
				result);
	}
	
	@Test
	public void canGetNodeMembershipWhereTheresOnlyAffiliation() throws Exception {
		dbTester.loadData("node_1");

		store.deleteUserSubscriptions(TEST_SERVER1_USER1_JID);
		
		NodeMembership result = store.getNodeMembership(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

		NodeMembership expected = new NodeMembershipImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.none, Affiliations.owner);

		assertEquals("An unexpected node membership was returned", expected,
				result);
	}
	
	
	@Test
	public void canGetNodeMembershipWhereTheresNoMembership() throws Exception {

		
		NodeMembership result = store.getNodeMembership(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID);

		NodeMembership expected = new NodeMembershipImpl(
				TEST_SERVER1_NODE1_ID, TEST_SERVER1_USER1_JID,
				Subscriptions.none, Affiliations.none);

		assertEquals("An unexpected node membership was returned", expected,
				result);
	}
}