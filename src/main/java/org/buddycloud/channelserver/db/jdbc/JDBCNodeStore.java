package org.buddycloud.channelserver.db.jdbc;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Date;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.channel.Conf;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.exception.ItemNotFoundException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeMembership;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.NodeThread;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeMembershipImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeThreadImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;
import org.xmpp.resultsetmanagement.ResultSet;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class JDBCNodeStore implements NodeStore {

	private Logger logger = Logger.getLogger(JDBCNodeStore.class);
	private final Connection conn;
	private final NodeStoreSQLDialect dialect;
	private final Deque<JDBCTransaction> transactionStack;
	private boolean transactionHasBeenRolledBack = false;

	/**
	 * Create a new node store connection backed by the given JDBC
	 * {@link Connection}.
	 * 
	 * @param conn
	 *            the connection to the backing database.
	 */
	public JDBCNodeStore(final Connection conn,
			final NodeStoreSQLDialect dialect) {
		this.conn = conn;
		this.dialect = dialect;
		transactionStack = new ArrayDeque<JDBCTransaction>();
	}

	@Override
	public void createNode(JID owner, String nodeId,
			Map<String, String> nodeConf) throws NodeStoreException {
		if (owner == null)
			throw new NullPointerException("owner must not be null");
		if (nodeId == null)
			throw new NullPointerException("nodeId must not be null");

		PreparedStatement addStatement = null;
		try {
			// Store node
			addStatement = conn.prepareStatement(dialect.insertNode());
			addStatement.setString(1, nodeId);
			addStatement.executeUpdate();
			addStatement.close();

			// Store the config (if there is any)
			if (nodeConf != null) {
				setNodeConf(nodeId, nodeConf);
			}
			NodeSubscriptionImpl subscription = new NodeSubscriptionImpl(
					nodeId, owner, Subscriptions.subscribed, null);
			addUserSubscription(subscription);
			setUserAffiliation(nodeId, owner, Affiliations.owner);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(addStatement);
		}
	}

	@Override
	public void deleteNode(String nodeId) throws NodeStoreException {
		PreparedStatement deleteStatement = null;
		try {
			deleteStatement = conn.prepareStatement(dialect.deleteNode());
			deleteStatement.setString(1, nodeId);
			deleteStatement.executeUpdate();
			deleteStatement.close();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(deleteStatement);
		}
	}

	@Override
	public void addRemoteNode(String nodeId) throws NodeStoreException {
		if (null == nodeId) {
			throw new NullPointerException("nodeId must not be null");
		}
		PreparedStatement addStatement = null;
		try {
			addStatement = conn.prepareStatement(dialect.insertNode());
			addStatement.setString(1, nodeId);
			addStatement.executeUpdate();
			addStatement.close();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(addStatement);
		}
	}

	@Override
	public void setNodeConfValue(String nodeId, String key, String value)
			throws NodeStoreException {
		if (nodeId == null)
			throw new NullPointerException("nodeId must not be null");
		if (key == null)
			throw new NullPointerException("key must not be null");

		PreparedStatement updateStatement = null;
		PreparedStatement addStatement = null;

		try {
			updateStatement = conn.prepareStatement(dialect.updateNodeConf());
			updateStatement.setString(1, value);
			updateStatement.setString(2, nodeId);
			updateStatement.setString(3, key);
			int rows = updateStatement.executeUpdate();
			updateStatement.close();

			if (rows == 0) { // If the update didn't update any rows
				addStatement = conn.prepareStatement(dialect.insertConf());
				addStatement.setString(1, nodeId);
				addStatement.setString(2, key);
				addStatement.setString(3, value);

				addStatement.executeUpdate();
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(updateStatement);
			close(addStatement);
		}
	}

	@Override
	public void setNodeConf(String nodeId, Map<String, String> conf)
			throws NodeStoreException {
		Transaction t = null;

		try {
			t = beginTransaction();

			for (final Entry<String, String> entry : conf.entrySet()) {
				setNodeConfValue(nodeId, entry.getKey(), entry.getValue());
			}
			t.commit();
		} catch (NodeStoreException e) {
			throw new NodeStoreException(e);
		} finally {
			close(t);
		}
	}

	@Override
	public boolean nodeExists(String nodeId) throws NodeStoreException {
		PreparedStatement existsStatement = null;
		try {
			existsStatement = conn.prepareStatement(dialect.nodeExists());
			existsStatement.setString(1, nodeId);
			java.sql.ResultSet rs = existsStatement.executeQuery();

			boolean exists = rs.next();

			rs.close();
			existsStatement.close();

			return exists;
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(existsStatement); // Will implicitly close the resultset if
									// required
		}
	}

	@Override
	public void setUserAffiliation(String nodeId, JID user,
			Affiliations affiliation) throws NodeStoreException {
		PreparedStatement deleteStatement = null;
		PreparedStatement updateStatement = null;
		PreparedStatement addStatement = null;
		Transaction t = null;

		try {
			t = beginTransaction();
			if (affiliation.equals(Affiliations.none)) {
				deleteStatement = conn.prepareStatement(dialect
						.deleteAffiliation());
				deleteStatement.setString(1, nodeId);
				deleteStatement.setString(2, user.toBareJID());
				deleteStatement.executeUpdate();
				deleteStatement.close();
			} else {
				updateStatement = conn.prepareStatement(dialect
						.updateAffiliation());
				updateStatement.setString(1, affiliation.toString());
				updateStatement.setString(2, nodeId);
				updateStatement.setString(3, user.toBareJID());
				int rows = updateStatement.executeUpdate();
				updateStatement.close();

				if (rows == 0) { // If the update didn't update any rows
					addStatement = conn.prepareStatement(dialect
							.insertAffiliation());
					addStatement.setString(1, nodeId);
					addStatement.setString(2, user.toBareJID());
					addStatement.setString(3, affiliation.toString());
					addStatement.executeUpdate();
					addStatement.close();
				}
			}
			t.commit();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(deleteStatement);
			close(updateStatement);
			close(addStatement);
			close(t);
		}
	}

	@Override
	public String getNodeConfValue(String nodeId, String key)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.selectSingleNodeConfValue());

			stmt.setString(1, nodeId);
			stmt.setString(2, key);

			java.sql.ResultSet rs = stmt.executeQuery();

			String result = null;

			if (rs.next()) {
				result = rs.getString(1);
			}

			return result;
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void deleteNodeConfiguration(String nodeId)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.deleteConfFromNode());
			stmt.setString(1, nodeId);
			stmt.executeUpdate();
			stmt.close();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public Map<String, String> getNodeConf(String nodeId)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.selectNodeConf());

			stmt.setString(1, nodeId);

			java.sql.ResultSet rs = stmt.executeQuery();

			HashMap<String, String> result = new HashMap<String, String>();

			while (rs.next()) {
				result.put(rs.getString(1), rs.getString(2));
			}

			return result;
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void addUserSubscription(final NodeSubscription subscription)
			throws NodeStoreException {
		PreparedStatement deleteStatement = null;
		PreparedStatement updateStatement = null;
		PreparedStatement addStatement = null;
		Transaction t = null;

		try {
			if (subscription.getSubscription().equals(Subscriptions.none)) {
				deleteStatement = conn.prepareStatement(dialect
						.deleteSubscription());
				deleteStatement.setString(1, subscription.getNodeId());
				deleteStatement
						.setString(2, subscription.getUser().toBareJID());
				deleteStatement.executeUpdate();
				deleteStatement.close();
			} else {
				t = beginTransaction();
				updateStatement = conn.prepareStatement(dialect
						.updateSubscription());
				updateStatement.setString(1, subscription.getSubscription()
						.toString());
				updateStatement.setString(2, subscription.getListener()
						.toString());
				updateStatement.setString(3, subscription.getNodeId());
				updateStatement
						.setString(4, subscription.getUser().toBareJID());

				int rows = updateStatement.executeUpdate();
				updateStatement.close();

				if (rows == 0) { // If the update didn't update any rows
					addStatement = conn.prepareStatement(dialect
							.insertSubscription());
					addStatement.setString(1, subscription.getNodeId());
					addStatement.setString(2, subscription.getUser()
							.toBareJID());
					addStatement.setString(3, subscription.getListener()
							.toString());
					addStatement.setString(4, subscription.getSubscription()
							.toString());
					if (null == subscription.getInvitedBy()) {
						addStatement.setNull(5, Types.NULL);
					} else {
					    addStatement.setString(5, subscription.getInvitedBy().toBareJID());
					}
					addStatement.executeUpdate();
					addStatement.close();
				}
				t.commit();
			}
		} catch (SQLException e) {
			logger.debug("Error adding new subscription: " + e.getMessage());
			throw new NodeStoreException(e);
		} finally {
			close(deleteStatement);
			close(updateStatement);
			close(addStatement);
			close(t);
		}
	}

	@Override
	public ResultSet<NodeAffiliation> getAffiliationChanges(JID user,
			Date startDate, Date endDate) throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.selectAffiliationChanges());
			stmt.setString(3, user.toBareJID());
			stmt.setTimestamp(1, new java.sql.Timestamp(startDate.getTime()));
			stmt.setTimestamp(2, new java.sql.Timestamp(endDate.getTime()));

			java.sql.ResultSet rs = stmt.executeQuery();

			ArrayList<NodeAffiliation> result = new ArrayList<NodeAffiliation>();

			while (rs.next()) {
				NodeAffiliationImpl nodeSub = new NodeAffiliationImpl(
						rs.getString(1), new JID(rs.getString(2)),
						Affiliations.valueOf(rs.getString(3)),
						rs.getTimestamp(4));
				result.add(nodeSub);
			}

			return new ResultSetImpl<NodeAffiliation>(result);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}
	
    @Override
	public ResultSet<NodeMembership> getUserMemberships(JID jid) throws NodeStoreException {

		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect
					.selectUserMemberships());
			stmt.setString(1, jid.toBareJID());
			stmt.setString(2, jid.toBareJID());

			java.sql.ResultSet rs = stmt.executeQuery();

			ArrayList<NodeMembership> result = new ArrayList<NodeMembership>();
			while (rs.next()) {
				JID inviter = null;
				if (null != rs.getString(6)) {
					inviter = new JID(rs.getString(6));
				}
				NodeMembershipImpl membership = new NodeMembershipImpl(
						rs.getString(1), new JID(rs.getString(2)), new JID(rs.getString(3)), Subscriptions.valueOf(rs.getString(4)), Affiliations.valueOf(rs
								.getString(5)), inviter, rs.getTimestamp(7));
				result.add(membership);
			}

			return new ResultSetImpl<NodeMembership>(result);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public ArrayList<JID> getNodeOwners(String node) throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect
					.selectNodeOwners());
			stmt.setString(1, node);

			java.sql.ResultSet rs = stmt.executeQuery();
			ArrayList<JID> result = new ArrayList<JID>();

			while (rs.next()) {
				result.add(new JID(rs.getString(1)));
			}

			return result;
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}
	
	@Override
	public NodeMembership getNodeMembership(String nodeId, JID user) throws NodeStoreException {
		PreparedStatement selectStatement = null;

		try {
			NodeMembershipImpl membership;

			selectStatement = conn.prepareStatement(dialect
					.selectMembership());
			selectStatement.setString(2, nodeId);
			selectStatement.setString(1, user.toBareJID());
			selectStatement.setString(4, nodeId);
			selectStatement.setString(3, user.toBareJID());
			
			java.sql.ResultSet rs = selectStatement.executeQuery();

			if (rs.next()) {
				JID inviter = null;
				if (null != rs.getString(6)) {
					inviter = new JID(rs.getString(6));
				}
				membership = new NodeMembershipImpl(nodeId, new JID(
						rs.getString(2)), new JID(rs.getString(3)),
						Subscriptions.valueOf(rs.getString(4)),
						Affiliations.valueOf(rs.getString(5)),
						inviter, rs.getTimestamp(7));
			} else {
				membership = new NodeMembershipImpl(nodeId, user, user,
						Subscriptions.none, Affiliations.none, null);
			}

			return membership;
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(selectStatement); // Will implicitly close the resultset if
									// required
		}
	}
	
	@Override
	public ResultSet<NodeMembership> getNodeMemberships(String nodeId) throws NodeStoreException {

		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect
					.selectNodeMemberships());
			stmt.setString(1, nodeId);
			stmt.setString(2, nodeId);

			java.sql.ResultSet rs = stmt.executeQuery();

			ArrayList<NodeMembership> result = new ArrayList<NodeMembership>();
			while (rs.next()) {
				JID inviter = null;
				if (null != rs.getString(6)) {
					inviter = new JID(rs.getString(6));
				}
				NodeMembershipImpl membership = new NodeMembershipImpl(
						rs.getString(1), new JID(rs.getString(2)), new JID(rs.getString(3)), Subscriptions.valueOf(rs.getString(4)), Affiliations.valueOf(rs
								.getString(5)), inviter, rs.getTimestamp(7));
				result.add(membership);
			}

			return new ResultSetImpl<NodeMembership>(result);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public ResultSet<NodeSubscription> getSubscriptionChanges(JID user,
			Date startDate, Date endDate) throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.getSubscriptionChanges());
			stmt.setTimestamp(1, new java.sql.Timestamp(startDate.getTime()));
			stmt.setTimestamp(2, new java.sql.Timestamp(endDate.getTime()));
			stmt.setString(3, user.toBareJID());

			java.sql.ResultSet rs = stmt.executeQuery();

			ArrayList<NodeSubscription> result = new ArrayList<NodeSubscription>();

			while (rs.next()) {
				JID invitedBy = null;
				if (null != rs.getString(5)) {
					invitedBy = new JID(rs.getString(5));
				}
				NodeSubscriptionImpl nodeSub = new NodeSubscriptionImpl(
						rs.getString(1), new JID(rs.getString(2)), new JID(
								rs.getString(3)), Subscriptions.valueOf(rs
								.getString(4)), invitedBy, rs.getTimestamp(6));
				result.add(nodeSub);
			}

			return new ResultSetImpl<NodeSubscription>(result);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public ResultSet<NodeSubscription> getNodeSubscriptionListeners(
			String nodeId) throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect
					.selectSubscriptionListenersForNode());
			stmt.setString(1, nodeId);

			java.sql.ResultSet rs = stmt.executeQuery();

			ArrayList<NodeSubscription> result = new ArrayList<NodeSubscription>();

			while (rs.next()) {
				NodeSubscriptionImpl nodeSub = new NodeSubscriptionImpl(
						rs.getString(2), new JID(rs.getString(1)),
						Subscriptions.valueOf(rs.getString(3)), null,
						rs.getTimestamp(4));
				result.add(nodeSub);
			}

			return new ResultSetImpl<NodeSubscription>(result);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public ResultSet<NodeSubscription> getNodeSubscriptionListeners()
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect
					.selectSubscriptionListeners());
			
			java.sql.ResultSet rs = stmt.executeQuery();

			ArrayList<NodeSubscription> result = new ArrayList<NodeSubscription>();

			while (rs.next()) {
				NodeSubscriptionImpl nodeSub = new NodeSubscriptionImpl(
						rs.getString(2), new JID(rs.getString(1)),
						Subscriptions.valueOf(rs.getString(3)), null,
						rs.getTimestamp(4));
				result.add(nodeSub);
			}

			return new ResultSetImpl<NodeSubscription>(result);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}
	
	@Override
	public CloseableIterator<NodeItem> getNodeItems(String nodeId,
			String afterItemId, int count) throws NodeStoreException {
		NodeItem afterItem = null;

		PreparedStatement stmt = null;

		if (afterItemId != null) {
			afterItem = getNodeItem(nodeId, afterItemId);
		}

		String countSQL = "";

		if (count > -1) {
			countSQL = " OFFSET 0 LIMIT " + count;
		} else if (count < -1) {
			throw new IllegalArgumentException(
					"Invalid value for parameter count: " + count);
		}

		try {
			if (afterItem == null) {
				stmt = conn.prepareStatement(dialect.selectItemsForNode()
						+ countSQL);
				stmt.setString(1, nodeId);

				java.sql.ResultSet rs = stmt.executeQuery();

				stmt = null; // Prevent the finally block from closing the
								// statement

				return new ResultSetIterator<NodeItem>(rs,
						new ResultSetIterator.RowConverter<NodeItem>() {
							@Override
							public NodeItem convertRow(java.sql.ResultSet rs)
									throws SQLException {
								return new NodeItemImpl(rs.getString(1),
										rs.getString(2), rs.getTimestamp(3),
										rs.getString(4), rs.getString(5));
							}
						});
			} else {
				stmt = conn.prepareStatement(dialect
						.selectItemsForNodeBeforeDate() + countSQL);
				stmt.setString(1, nodeId);
				stmt.setTimestamp(2, new java.sql.Timestamp(afterItem
						.getUpdated().getTime()));
				stmt.setTimestamp(3, new java.sql.Timestamp(afterItem
						.getUpdated().getTime()));
				stmt.setString(4, getLocalId(afterItemId));

				java.sql.ResultSet rs = stmt.executeQuery();

				return new ResultSetIterator<NodeItem>(rs,
						new ResultSetIterator.RowConverter<NodeItem>() {
							@Override
							public NodeItem convertRow(java.sql.ResultSet rs)
									throws SQLException {
								return new NodeItemImpl(rs.getString(1),
										rs.getString(2), rs.getTimestamp(3),
										rs.getString(4), rs.getString(5));
							}
						});
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		}
	}

	@Override
	public CloseableIterator<NodeItem> getFirehose(int limit,
			String afterItemId, boolean isAdmin) throws NodeStoreException {

		PreparedStatement stmt = null;
		Date beforeDate = null;

//		if (afterItemId != null) {
//			beforeDate = getNodeItemById(afterItemId).getUpdated();
//		} else {
			beforeDate = new Date();
			afterItemId = "";
//		}

		if (limit < 0) {
			throw new IllegalArgumentException(
					"Invalid value for parameter count: " + limit);
		}

		String accessModel = "open";
		if (true == isAdmin)
			accessModel = "%";
		try {
			stmt = conn.prepareStatement(dialect
					.selectItemsForLocalNodesBeforeDate());
			stmt.setTimestamp(1, new java.sql.Timestamp(beforeDate.getTime()));
			stmt.setString(2, Conf.ACCESS_MODEL);
			stmt.setString(3, accessModel);
			stmt.setString(
					4,
					"%@"
							+ Configuration.getInstance().getProperty(
									Configuration.CONFIGURATION_SERVER_DOMAIN)
							+ "%");
			stmt.setString(
					5,
					"%@"
							+ Configuration
									.getInstance()
									.getProperty(
											Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN)
							+ "%");
			stmt.setInt(6, limit);

			java.sql.ResultSet rs = stmt.executeQuery();

			LinkedList<NodeItem> results = new LinkedList<NodeItem>();

			while (rs.next()) {
				results.push(new NodeItemImpl(rs.getString(1), rs.getString(2),
						rs.getTimestamp(3), rs.getString(4), rs.getString(5)));
			}

			return new ClosableIteratorImpl<NodeItem>(results.iterator());
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public int getFirehoseItemCount(boolean isAdmin) throws NodeStoreException {
		PreparedStatement stmt = null;

		String accessModel = "open";
		if (true == isAdmin)
			accessModel = "%";

		try {
			stmt = conn.prepareStatement(dialect.countItemsForLocalNodes());
			stmt.setString(1, Conf.ACCESS_MODEL);
			stmt.setString(2, accessModel);
			stmt.setString(
					3,
					"%@"
							+ Configuration.getInstance().getProperty(
									Configuration.CONFIGURATION_SERVER_DOMAIN)
							+ "%");
			stmt.setString(
					4,
					"%@"
							+ Configuration
									.getInstance()
									.getProperty(
											Configuration.CONFIGURATION_SERVER_TOPICS_DOMAIN)
							+ "%");
			java.sql.ResultSet rs = stmt.executeQuery();

			if (rs.next()) {
				return rs.getInt(1);
			} else {
				return 0; // This really shouldn't happen!
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if
							// required
		}
	}

	@Override
	public int getCountRecentItems(JID user, Date since, int maxPerNode,
			String node, boolean parentOnly) throws NodeStoreException {

		if (null == node)
			node = "/posts";
		if (-1 == maxPerNode)
			maxPerNode = Integer.MAX_VALUE;

		String queryPart;
		String parentOnlyReplacement;
		PreparedStatement stmt = null;

		try {
			ResultSet<NodeMembership> subscriptions = this
					.getUserMemberships(user);

			ArrayList<String> queryParts = new ArrayList<String>();
			ArrayList<Object> parameters = new ArrayList<Object>();

			for (NodeMembership subscription : subscriptions) {
				if (false == subscription.getSubscription().equals(
						Subscriptions.subscribed))
					continue;
				if (false == subscription
						.getNodeId()
						.substring(
								subscription.getNodeId().length()
										- node.length()).equals(node))
					continue;
		        String parentQuery = "%";
		        queryPart = dialect.selectCountRecentItemParts();
		        parentOnlyReplacement = "";
		        if (true == parentOnly) {
		        	parentOnlyReplacement = "AND \"in_reply_to\" IS NULL";
		        }
		        queryPart = queryPart.replace("%parentOnly%", parentOnlyReplacement);
		        queryParts.add(queryPart);
				
				parameters.add(subscription.getNodeId());
				parameters.add(new java.sql.Timestamp(since.getTime()));
				parameters.add(maxPerNode);
			}
			stmt = conn.prepareStatement(StringUtils.join(queryParts,
					" UNION ALL "));
			int index = 1;
			for (Object parameter : parameters) {
				stmt.setObject(index, parameter);
				++index;
			}

			java.sql.ResultSet rs = stmt.executeQuery();
			int count = 0;
			while (rs.next()) {
				count += rs.getInt(1);
			}
			stmt = null; // Prevent the finally block from closing the
							// statement

			return count;
		} catch (SQLException e) {
			logger.error(e);
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public CloseableIterator<NodeItem> getRecentItems(JID user, Date since,
			int maxPerNode, int limit, GlobalItemID afterItemId, String node, boolean parentOnly)
			throws NodeStoreException {

		PreparedStatement stmt = null;

		try {
			
			if (null == node)
				node = "/posts";
			if (-1 == limit)
				limit = 50;
			if (-1 == maxPerNode)
				maxPerNode = Integer.MAX_VALUE;

	        String queryPart;
	        String parentOnlyReplacement;

			ResultSet<NodeMembership> subscriptions = this
					.getUserMemberships(user);

			ArrayList<String> queryParts = new ArrayList<String>();
			ArrayList<Object> parameters = new ArrayList<Object>();

			int counter = 1;
			for (NodeMembership subscription : subscriptions) {
				if (false == subscription.getSubscription().equals(
						Subscriptions.subscribed))
					continue;
				if (false == subscription
						.getNodeId().endsWith(node))
					continue;
		        queryPart = dialect.selectRecentItemParts();
		        parentOnlyReplacement = "";
		        if (true == parentOnly) {
		        	parentOnlyReplacement = "AND \"in_reply_to\" IS NULL ";
		        }
		        queryPart = queryPart.replace("%parentOnly%", parentOnlyReplacement);
		        queryPart = queryPart.replace(
						"%counter%", String.valueOf(counter));
		        queryParts.add(queryPart);
				parameters.add(subscription.getNodeId());
				parameters.add(new java.sql.Timestamp(since.getTime()));
				parameters.add(maxPerNode);
				++counter;
			}
			
			String sql = "SELECT * FROM ("
				+ StringUtils.join(queryParts, " UNION ALL ")
				+ ") AS recentItemsQuery";
			
			if (afterItemId != null) {
				NodeItem afterItem = getNodeItem(afterItemId.getNodeID(), afterItemId.getItemID());
				
				if(afterItem != null) {
					sql += " WHERE \"updated\" < ? OR" +
							" ( \"updated\" = ? AND" +
							" ( \"node\" > ? OR" +
							" ( \"node\" = ? AND \"id\" > ? ) ) )";
					
					parameters.add(new Timestamp(afterItem.getUpdated().getTime()));
					parameters.add(new Timestamp(afterItem.getUpdated().getTime()));
					parameters.add(afterItem.getNodeId());
					parameters.add(afterItem.getNodeId());
					parameters.add(afterItem.getId());
				}
			}
			
			sql += " ORDER BY \"updated\" DESC, \"node\" ASC, \"id\" ASC LIMIT ?;";
			
			stmt = conn
					.prepareStatement(sql.toString());
			int index = 1;
			for (Object parameter : parameters) {
				stmt.setObject(index, parameter);
				++index;
			}
			stmt.setInt(index, limit);
			java.sql.ResultSet rs = stmt.executeQuery();

			stmt = null; // Prevent the finally block from closing the
							// statement
			ArrayList<NodeItem> results = new ArrayList<NodeItem>();
			while (rs.next()) {
				results.add(new NodeItemImpl(rs.getString(2), rs.getString(1),
						rs.getTimestamp(4), rs.getString(3), rs.getString(5)));
			}
			return new ClosableIteratorImpl<NodeItem>(results.iterator());
		} catch (SQLException e) {
			e.printStackTrace();
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public CloseableIterator<NodeItem> getNodeItems(String nodeId)
			throws NodeStoreException {
		return getNodeItems(nodeId, null, -1);
	}

	@Override
	public ClosableIteratorImpl<NodeItem> getNodeItemReplies(String nodeId,
			String itemId, String afterItemId, int limit)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			Date since = new Date(0);
			if (null != afterItemId)
				since = getNodeItem(nodeId, afterItemId).getUpdated();

			String query = dialect.selectItemReplies();
			if (-1 != limit) {
				query += " LIMIT ?";
			}
			stmt = conn.prepareStatement(query);
			stmt.setString(1, nodeId);
			stmt.setString(2, "%" + getLocalId(itemId));
			stmt.setTimestamp(3, new java.sql.Timestamp(since.getTime()));
			if (-1 != limit)
				stmt.setInt(4, limit);

			java.sql.ResultSet rs = stmt.executeQuery();

			LinkedList<NodeItem> results = new LinkedList<NodeItem>();

			while (rs.next()) {
				results.push(new NodeItemImpl(rs.getString(2), rs.getString(1),
						rs.getTimestamp(4), rs.getString(3), rs.getString(5)));
			}

			return new ClosableIteratorImpl<NodeItem>(results.iterator());
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public int getCountNodeItemReplies(String nodeId, String itemId)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.selectCountItemReplies());
			stmt.setString(1, nodeId);
			stmt.setString(2, "%" + getLocalId(itemId));

			java.sql.ResultSet rs = stmt.executeQuery();

			if (rs.next()) {
				return rs.getInt(1);
			} else {
				return 0; // This really shouldn't happen!
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public ClosableIteratorImpl<NodeItem> getNodeItemThread(String nodeId,
			String itemId, String afterItemId, int limit)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			Date since = new Date(0);
			if (null != afterItemId)
				since = getNodeItem(nodeId, afterItemId).getUpdated();

			String query = dialect.selectItemThread();
			if (-1 != limit) {
				query += " LIMIT ?";
			}
			stmt = conn.prepareStatement(query);
			stmt.setString(1, nodeId);
			itemId = getLocalId(itemId);
			stmt.setString(2, "%" + itemId);
			stmt.setString(3, itemId);
			stmt.setTimestamp(4, new java.sql.Timestamp(since.getTime()));
			if (-1 != limit)
				stmt.setInt(5, limit);

			java.sql.ResultSet rs = stmt.executeQuery();

			LinkedList<NodeItem> results = new LinkedList<NodeItem>();

			while (rs.next()) {
				results.push(new NodeItemImpl(rs.getString(2), rs.getString(1),
						rs.getTimestamp(4), rs.getString(3), rs.getString(5)));
			}

			return new ClosableIteratorImpl<NodeItem>(results.iterator());
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public int getCountNodeThread(String nodeId, String itemId)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.selectCountItemThread());
			stmt.setString(1, nodeId);
			itemId = getLocalId(itemId);
			stmt.setString(2, "%" + itemId);
			stmt.setString(3, itemId);

			java.sql.ResultSet rs = stmt.executeQuery();

			if (rs.next()) {
				return rs.getInt(1);
			} else {
				return 0; // This really shouldn't happen!
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public CloseableIterator<NodeItem> getNewNodeItemsForUser(JID user,
			Date startDate, Date endDate) throws NodeStoreException {

		PreparedStatement stmt = null;

		try {

			stmt = conn.prepareStatement(dialect
					.selectItemsForUsersNodesBetweenDates());
			stmt.setString(3, user.toBareJID());
			stmt.setTimestamp(1, new java.sql.Timestamp(startDate.getTime()));
			stmt.setTimestamp(2, new java.sql.Timestamp(endDate.getTime()));

			java.sql.ResultSet rs = stmt.executeQuery();

			LinkedList<NodeItem> results = new LinkedList<NodeItem>();

			while (rs.next()) {
				results.push(new NodeItemImpl(rs.getString(1), rs.getString(2),
						rs.getTimestamp(3), rs.getString(4), rs.getString(5)));
			}

			return new ClosableIteratorImpl<NodeItem>(results.iterator());
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public int countNodeItems(String nodeId) throws NodeStoreException {
		PreparedStatement selectStatement = null;

		try {
			selectStatement = conn
					.prepareStatement(dialect.countItemsForNode());
			selectStatement.setString(1, nodeId);

			java.sql.ResultSet rs = selectStatement.executeQuery();

			if (rs.next()) {
				return rs.getInt(1);
			} else {
				return 0; // This really shouldn't happen!
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(selectStatement); // Will implicitly close the resultset if
									// required
		}
	}

	@Override
	public NodeItem getNodeItem(String nodeId, String nodeItemId)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.selectSingleItem());
			
			stmt.setString(1, nodeId);
			stmt.setString(2, getLocalId(nodeItemId));

			java.sql.ResultSet rs = stmt.executeQuery();

			if (rs.next()) {
				return new NodeItemImpl(rs.getString(1), rs.getString(2),
						rs.getTimestamp(3), rs.getString(4), rs.getString(5));
			}
			return null;
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void addNodeItem(NodeItem item) throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.insertItem());

			stmt.setString(1, item.getNodeId());
			stmt.setString(2, item.getId());
			stmt.setTimestamp(3, new Timestamp(item.getUpdated().getTime()));
			stmt.setString(4, item.getPayload());
			stmt.setString(5, item.getInReplyTo());

			stmt.executeUpdate();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void updateNodeItem(NodeItem item) throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.updateItem());

			stmt.setTimestamp(1, new Timestamp(item.getUpdated().getTime()));
			stmt.setString(2, item.getPayload());
			stmt.setString(3, item.getNodeId());
			stmt.setString(4, item.getId());

			int rows = stmt.executeUpdate();

			if (rows != 1) {
				throw new ItemNotFoundException(
						"No records affected when updating an item");
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}
	
	@Override
	public void updateThreadParent(String node, String itemId) throws NodeStoreException {
		PreparedStatement stmt = null;
		try {
			stmt = conn.prepareStatement(dialect.updateThreadParent());

			stmt.setString(1, node);
			stmt.setString(2, itemId);

			stmt.executeUpdate();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void deleteNodeItemById(String nodeId, String nodeItemId)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.deleteItem());

			stmt.setString(1, nodeId);
			stmt.setString(2, getLocalId(nodeItemId));

			int rows = stmt.executeUpdate();

			if (rows != 1) {
				throw new ItemNotFoundException(
						"No records affected when deleting an item");
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void purgeNodeItems(String nodeId) throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.deleteItems());
			stmt.setString(1, nodeId);
			stmt.executeUpdate();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}
	
	@Override
	public CloseableIterator<NodeItem> performSearch(JID searcher,
			List content, JID author, int page, int rpp) throws NodeStoreException {
		PreparedStatement stmt = null;
		try {
			String sql = "SELECT * FROM \"items\"  " +
	            "LEFT JOIN \"subscriptions\" ON \"items\".\"node\" = \"subscriptions\".\"node\" " +
				"WHERE " +
	            "\"subscriptions\".\"user\" = ?  " +
				"AND \"subscriptions\".\"subscription\" = 'subscribed' " +
	            "AND RIGHT(\"items\".\"node\", 6) = '/posts' " +
	            " $searchParameters " +
	            "ORDER BY \"items\".\"updated\" DESC " +
	            "LIMIT ? OFFSET ?;";
			ArrayList<String> parameterValues = new ArrayList<String>();
			parameterValues.add(searcher.toBareJID());
			String searchParameters = "";
			
			for (String term : (List<String>) content) {
				searchParameters += "AND (\"items\".\"xml\" LIKE ?)  ";
				parameterValues.add("%<content%>%" + term + "%</content>%");
			}

			if (null != author) {
				searchParameters += "AND (\"items\".\"xml\" LIKE ?)";
				parameterValues.add("%<name>" + author.toBareJID() + "</name>%");
			}
			stmt = conn.prepareStatement(sql.replace("$searchParameters", searchParameters));
		
			int counter = 0;
			for (String value : parameterValues) {
				++counter;
				stmt.setString(counter, value);
			}
			stmt.setInt(parameterValues.size() + 1, rpp);
			stmt.setInt(parameterValues.size() + 2, (page - 1) * rpp);
			
			java.sql.ResultSet rs = stmt.executeQuery();

			stmt = null; // Prevent the finally block from closing the
							// statement

			return new ResultSetIterator<NodeItem>(rs,
					new ResultSetIterator.RowConverter<NodeItem>() {
						@Override
						public NodeItem convertRow(java.sql.ResultSet rs)
								throws SQLException {
							return new NodeItemImpl(rs.getString(1),
									rs.getString(2), rs.getTimestamp(3),
									rs.getString(4), rs.getString(5));
						}
					});
		} catch (SQLException e) {
			e.printStackTrace();
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public ArrayList<String> getNodeList() throws NodeStoreException {

		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(dialect.selectNodeList());

			java.sql.ResultSet rs = stmt.executeQuery();

			ArrayList<String> result = new ArrayList<String>();

			while (rs.next()) {
				result.add(rs.getString(1));
			}
			return result;
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public boolean isCachedNode(String nodeId) throws NodeStoreException {
		return ((this.countNodeItems(nodeId) > 0) &&
				(true == this.isCachedNodeConfig(nodeId)));
	}
	
	@Override
	public boolean isCachedNodeConfig(String nodeId) throws NodeStoreException {
		return (this.getNodeConf(nodeId).size() > 0);
	}

	@Override
	public boolean isCachedJID(JID jid) throws NodeStoreException {
		PreparedStatement selectStatement = null;
		try {
			selectStatement = conn.prepareStatement(dialect
					.countSubscriptionsForJid());
			selectStatement.setString(1, jid.toBareJID());

			java.sql.ResultSet rs = selectStatement.executeQuery();

			if (rs.next()) {
				return (rs.getInt(1) > 0);
			} else {
				return false; // This really shouldn't happen!
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(selectStatement); // Will implicitly close the resultset if
									// required
		}
	}
		 
		
	@Override
	public boolean userHasRatedPost(String node, JID user, GlobalItemID id) throws NodeStoreException {
		PreparedStatement selectStatement = null;
		try {
			selectStatement = conn.prepareStatement(dialect.selectUserRatingsForAPost());
			selectStatement.setString(1, node);
			selectStatement.setString(2, "%<uri>acct:" + user.toBareJID() + "</uri>%");
            selectStatement.setString(3, "%<activity:target><id>" + id.toString() + "</id>%");
            
			java.sql.ResultSet rs = selectStatement.executeQuery();

			if (rs.next()) {
				return true;
			} else {
				return false; // This really shouldn't happen!
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(selectStatement); // Will implicitly close the resultset if
									// required
		}
	}

	@Override
	public boolean nodeHasSubscriptions(String nodeId)
			throws NodeStoreException {
		return (this.getNodeMemberships(nodeId).size() > 0);
	}

	@Override
	public ResultSet<NodeItem> getUserItems(JID userJid) throws NodeStoreException {
		PreparedStatement stmt = null;
		try {
			stmt = conn.prepareStatement(dialect.getUserItems());
			stmt.setString(1, userJid.toBareJID());
			java.sql.ResultSet rs = stmt.executeQuery();
			ArrayList<NodeItem> result = new ArrayList<NodeItem>();
			while (rs.next()) {
				NodeItem nodeItem = new NodeItemImpl(rs.getString(1),
						rs.getString(2), rs.getTimestamp(3),
						rs.getString(4), rs.getString(5));
				result.add(nodeItem);
			}
			return new ResultSetImpl<NodeItem>(result);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void deleteUserItems(JID userJid) throws NodeStoreException {
		PreparedStatement stmt = null;
		try {
			stmt = conn.prepareStatement(dialect.deleteUserItems());
			stmt.setString(1, userJid.toBareJID());
			stmt.executeUpdate();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void deleteUserAffiliations(JID userJid) throws NodeStoreException {
		PreparedStatement stmt = null;
		try {
			stmt = conn.prepareStatement(dialect.deleteUserAffiliations());
			stmt.setString(1, userJid.toBareJID());
			stmt.executeUpdate();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void deleteUserSubscriptions(JID userJid) throws NodeStoreException {
		PreparedStatement stmt = null;
		try {
			stmt = conn.prepareStatement(dialect.deleteUserSubscriptions());
			stmt.setString(1, userJid.toBareJID());
			stmt.executeUpdate();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}
	
	@Override
	public ResultSet<NodeThread> getNodeThreads(String node, String afterId,
			int limit) throws NodeStoreException {
		
		Date after = new Date();
		if (afterId != null) {
			NodeItem afterItem = getNodeItem(node, afterId);
			if (afterItem != null) {
				after = afterItem.getUpdated();
			}
		}
		
		PreparedStatement stmt = null;
		try {
			stmt = conn.prepareStatement(dialect.selectNodeThreads());
			stmt.setString(1, node);
			stmt.setTimestamp(2, new java.sql.Timestamp(after.getTime()));
			stmt.setInt(3, limit);
			
			java.sql.ResultSet rs = stmt.executeQuery();
			ArrayList<NodeThread> nodeThreads = new ArrayList<NodeThread>();
			
			NodeThreadImpl currentThread = null;
			while (rs.next()) {
				NodeItem nodeItem = new NodeItemImpl(rs.getString(1),
						rs.getString(2), rs.getTimestamp(3),
						rs.getString(4), rs.getString(5));
				String threadId = rs.getString(6);
				Date threadUpdated = rs.getTimestamp(7);
				if (currentThread == null || !threadId.equals(currentThread.getId())) {
					NodeThreadImpl newThread = new NodeThreadImpl(threadId, threadUpdated);
					nodeThreads.add(newThread);
					currentThread = newThread;
				}
				currentThread.addItem(nodeItem);
			}
			return new ResultSetImpl<NodeThread>(nodeThreads);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}
	
	@Override
	public int countNodeThreads(String node) throws NodeStoreException {
		PreparedStatement selectStatement = null;
		try {
			selectStatement = conn
					.prepareStatement(dialect.countNodeThreads());
			selectStatement.setString(1, node);
			java.sql.ResultSet rs = selectStatement.executeQuery();
			if (rs.next()) {
				return rs.getInt(1);
			} else {
				return 0; // This really shouldn't happen!
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(selectStatement);
		}
	}
	
	@Override
	public Transaction beginTransaction() throws NodeStoreException {
		if (transactionHasBeenRolledBack) {
			throw new IllegalStateException(
					"The transaction has already been rolled back");
		}

		JDBCTransaction transaction;
		try {
			transaction = new JDBCTransaction(this);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		}
		return transaction;
	}

	private void close(final Statement stmt) {
		if (stmt != null) {
			try {
				if (false == stmt.isClosed())
					stmt.close();
				// stmt.getConnection().close();
			} catch (SQLException e) {
				logger.error(
						"SQLException thrown while trying to close a statement",
						e);
			}
		}
	}

	private void close(final Transaction trans) throws NodeStoreException {
		if (trans != null) {
			trans.close();
		}
	}

	public class JDBCTransaction implements Transaction {
		private JDBCNodeStore store;
		private boolean closed;

		private JDBCTransaction(final JDBCNodeStore store) throws SQLException {
			this.store = store;
			closed = false;

			if (store.transactionStack.isEmpty()) {
				store.conn.setAutoCommit(false);
			}

			store.transactionStack.push(this);
		}

		@Override
		public void commit() throws NodeStoreException {
			if (closed) {
				throw new IllegalStateException(
						"Commit called on transaction that is already closed");
			}
			if (!isLatestTransaction()) {
				throw new IllegalStateException(
						"Commit called on transaction other than the innermost transaction");
			}
			if (store.transactionHasBeenRolledBack) {
				throw new IllegalStateException(
						"Commit called after inner transaction has already been rolled back");
			}

			store.transactionStack.pop();
			closed = true;

			try {
				if (store.transactionStack.isEmpty()) {
					store.conn.commit();
					store.conn.setAutoCommit(true);
					store.transactionHasBeenRolledBack = false;
				}
			} catch (SQLException e) {
				throw new NodeStoreException(e);
			}
		}

		@Override
		public void close() throws NodeStoreException {
			if (closed) {
				return; // Do nothing nicely and silently
			}

			if (!isLatestTransaction()) {
				throw new IllegalStateException(
						"Close called on transaction other than the innermost transaction");
			}

			store.transactionStack.pop();
			closed = true;
			store.transactionHasBeenRolledBack = true;

			try {
				if (store.transactionStack.isEmpty()) {
					store.conn.rollback();
					store.conn.setAutoCommit(true);
					store.transactionHasBeenRolledBack = false;
				}
			} catch (SQLException e) {
				throw new NodeStoreException(e);
			}
		}

		private boolean isLatestTransaction() {
			return (store.transactionStack.peek() == this);
		}
	}

	@Override
	public void close() throws NodeStoreException {
		try {
			conn.close();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		}
	}
	
	private String getLocalId(String nodeItemId) {
		if (GlobalItemIDImpl.isGlobalId(nodeItemId)) {
			nodeItemId = GlobalItemIDImpl.toLocalId(nodeItemId);
		}
		return nodeItemId;
	}

	public interface NodeStoreSQLDialect {
		String insertNode();

		String selectNodeMemberships();

		String selectUserMemberships();

		String selectMembership();

		String selectNodeOwners();

		String getUserItems();

		String selectItemsForLocalNodesBeforeDate();

		String countItemsForLocalNodes();

		String selectRecentItemParts();

		String countNodeAffiliations();
		
		String countNodeAffiliationsForOwner();

		String countUserAffiliations();

		String countSubscriptionsForNode();

		String countSubscriptionsToNodeForOwner();

		String deleteItems();

		String selectNodeList();

		String deleteNode();

		String countSubscriptionsForJid();

		String insertConf();

		String deleteConfFromNode();

		String updateNodeConf();

		String selectSingleNodeConfValue();

		String selectNodeConf();

		String selectAffiliation();

		String selectAffiliationsForUser();

		String selectAffiliationsForUserAfterNodeId();

		String selectAffiliationsForNode();
		
		String selectAffiliationsToNodeForOwner();

		String selectAffiliationsForNodeAfterJid();
		
		String selectAffiliationsToNodeForOwnerAfterJid();

		String selectAffiliationChanges();

		String insertAffiliation();

		String updateAffiliation();

		String deleteAffiliation();

		String selectSubscriptionsForUser();

		String selectSubscriptionsForUserAfterNode();

		String getSubscriptionChanges();

		String selectSubscriptionsToNodeForOwner();
		
		String selectSubscriptionsForNode();

		String selectSubscriptionsForNodeAfterJid();
		
		String selectSubscriptionListeners();

		String selectSubscriptionListenersForNode();

		String insertSubscription();

		String updateSubscription();

		String deleteSubscription();

		String nodeExists();

		String selectSingleItem();

		String selectItemsForNode();

		String selectItemsForNodeAfterDate();

		String selectItemsForNodeBeforeDate();

		String selectItemsForUsersNodesBetweenDates();

		String countItemsForNode();

		String selectItemReplies();

		String selectCountItemReplies();

		String selectItemThread();

		String selectCountItemThread();

		String insertItem();

		String updateItem();
		
		String updateThreadParent();

		String deleteItem();

		String selectCountRecentItemParts();
		
		String deleteUserItems();
		
		String deleteUserAffiliations();
		
		String deleteUserSubscriptions();

		String selectNodeThreads();
		
		String countNodeThreads();

		String selectUserRatingsForAPost();
	
	}

}