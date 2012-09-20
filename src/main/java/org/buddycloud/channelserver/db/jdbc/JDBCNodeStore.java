package org.buddycloud.channelserver.db.jdbc;

import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.exception.ItemNotFoundException;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.xmpp.packet.JID;

public class JDBCNodeStore implements NodeStore {
	private Logger logger = Logger.getLogger(JDBCNodeStore.class);
	private final Connection conn;
	private final Deque<JDBCTransaction> transactionStack;
	private boolean transactionHasBeenRolledBack = false;

	/**
	 * Create a new node store connection backed by the given JDBC
	 * {@link Connection}.
	 * 
	 * @param conn
	 *            the connection to the backing database.
	 */
	public JDBCNodeStore(final Connection conn) {
		this.conn = conn;
		transactionStack = new ArrayDeque<JDBCTransaction>();
	}

	@Override
	public void createNode(JID owner, String nodeId,
			Map<String, String> nodeConf) throws NodeStoreException {
		PreparedStatement addStatement = null;
		try {
			// Store node
			addStatement = conn.prepareStatement(SQL.ADD_NODE);
			addStatement.setString(1, nodeId);
			addStatement.executeUpdate();
			addStatement.close();

			// Store the config (if there is any)
			if (nodeConf != null) {
				for (Entry<String, String> entry : nodeConf.entrySet()) {
					setNodeConf(nodeId, entry.getKey(), entry.getValue());
				}
			}

			setUserAffiliation(nodeId, owner, Affiliations.owner);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(addStatement);
		}
	}

	@Override
	public void setNodeConf(String nodeId, String key, String value)
			throws NodeStoreException {
		PreparedStatement updateStatement = null;
		PreparedStatement addStatement = null;

		try {
			updateStatement = conn.prepareStatement(SQL.UPDATE_CONF);
			updateStatement.setString(1, value);
			updateStatement.setString(2, nodeId);
			updateStatement.setString(3, key);
			int rows = updateStatement.executeUpdate();
			updateStatement.close();

			if (rows == 0) { // If the update didn't update any rows
				addStatement = conn.prepareStatement(SQL.ADD_CONF);
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
	public boolean nodeExists(String nodeId) throws NodeStoreException {
		PreparedStatement existsStatement = null;
		try {
			existsStatement = conn.prepareStatement(SQL.NODE_EXISTS);
			existsStatement.setString(1, nodeId);
			ResultSet rs = existsStatement.executeQuery();

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

		try {
			if (affiliation.equals(Affiliations.none)) {
				deleteStatement = conn.prepareStatement(SQL.DELETE_AFFILIATION);
				deleteStatement.setString(1, nodeId);
				deleteStatement.setString(2, affiliation.toString());
				deleteStatement.executeUpdate();
				deleteStatement.close();
			} else {
				updateStatement = conn.prepareStatement(SQL.UPDATE_AFFILIATION);
				updateStatement.setString(1, affiliation.toString());
				updateStatement.setString(2, nodeId);
				updateStatement.setString(3, user.toString());
				int rows = updateStatement.executeUpdate();
				updateStatement.close();

				if (rows == 0) { // If the update didn't update any rows
					addStatement = conn.prepareStatement(SQL.ADD_AFFILIATION);
					addStatement.setString(1, nodeId);
					addStatement.setString(2, user.toString());
					addStatement.setString(3, affiliation.toString());
					addStatement.executeUpdate();
					addStatement.close();
				}
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(deleteStatement);
			close(updateStatement);
			close(addStatement);
		}
	}

	@Override
	public String getNodeConfValue(String nodeId, String key)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(SQL.GET_SINGLE_NODE_CONF_VALUE);

			stmt.setString(1, nodeId);
			stmt.setString(2, key);

			ResultSet rs = stmt.executeQuery();

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
	public void addUserSubscription(final NodeSubscription subscription)
			throws NodeStoreException {
		PreparedStatement deleteStatement = null;
		PreparedStatement updateStatement = null;
		PreparedStatement addStatement = null;

		try {
			if (subscription.getSubscription().equals(Subscriptions.none)) {
				deleteStatement = conn
						.prepareStatement(SQL.DELETE_SUBSCRIPTION);
				deleteStatement.setString(1, subscription.getNodeId());
				deleteStatement.setString(2, subscription.getUser().toString());
				deleteStatement.setString(3, subscription.getListener()
						.toString());
				deleteStatement.executeUpdate();
				deleteStatement.close();
			} else {
				updateStatement = conn
						.prepareStatement(SQL.UPDATE_SUBSCRIPTION);
				updateStatement.setString(1, subscription.getSubscription()
						.toString());
				updateStatement.setString(2, subscription.getNodeId());
				updateStatement.setString(3, subscription.getUser().toString());
				updateStatement.setString(4, subscription.getListener()
						.toString());
				int rows = updateStatement.executeUpdate();
				updateStatement.close();

				if (rows == 0) { // If the update didn't update any rows
					addStatement = conn.prepareStatement(SQL.ADD_SUBSCRIPTION);
					addStatement.setString(1, subscription.getNodeId());
					addStatement
							.setString(2, subscription.getUser().toString());
					addStatement.setString(3, subscription.getListener()
							.toString());
					addStatement.setString(4, subscription.getSubscription()
							.toString());
					addStatement.executeUpdate();
					addStatement.close();
				}
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(deleteStatement);
			close(updateStatement);
			close(addStatement);
		}
	}

	@Override
	public NodeAffiliation getUserAfilliation(String nodeId, JID user)
			throws NodeStoreException {
		PreparedStatement selectStatement = null;

		try {
			NodeAffiliationImpl affiliation;

			selectStatement = conn.prepareStatement(SQL.SELECT_AFFILIATION);
			selectStatement.setString(1, nodeId);
			selectStatement.setString(2, user.toString());

			ResultSet rs = selectStatement.executeQuery();

			if (rs.next()) {
				affiliation = new NodeAffiliationImpl(nodeId, user,
						Affiliations.valueOf(rs.getString(1)));
			} else {
				affiliation = new NodeAffiliationImpl(nodeId, user,
						Affiliations.none);
			}

			return affiliation;
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(selectStatement); // Will implicitly close the resultset if
									// required
		}
	}

	@Override
	public Collection<NodeSubscription> getUserSubscriptions(final JID user)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(SQL.SELECT_SUBSCRIPTIONS_FOR_USER);
			stmt.setString(1, user.toString());
			stmt.setString(2, user.toString());

			ResultSet rs = stmt.executeQuery();

			ArrayList<NodeSubscription> result = new ArrayList<NodeSubscription>();

			while (rs.next()) {
				NodeSubscriptionImpl nodeSub = new NodeSubscriptionImpl(
						rs.getString(1), new JID(rs.getString(2)), new JID(rs.getString(3)), Subscriptions.valueOf(rs
								.getString(4)));
				result.add(nodeSub);
			}

			return result;
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
				stmt = conn.prepareStatement(SQL.SELECT_ITEMS_FOR_NODE
						+ countSQL);
				stmt.setString(1, nodeId);

				ResultSet rs = stmt.executeQuery();

				stmt = null; // Prevent the finally block from closing the
								// statement

				return new ResultSetIterator<NodeItem>(rs,
						new ResultSetIterator.RowConverter<NodeItem>() {
							@Override
							public NodeItem convertRow(ResultSet rs)
									throws SQLException {
								return new NodeItemImpl(rs.getString(1),
										rs.getString(2), rs.getTimestamp(3),
										rs.getString(4));
							}
						});
			} else {
				stmt = conn
						.prepareStatement(SQL.SELECT_ITEMS_FOR_NODE_AFTER_DATE
								+ countSQL);
				stmt.setString(1, nodeId);
				stmt.setDate(2, new java.sql.Date(afterItem.getUpdated()
						.getTime()));
				stmt.setDate(3, new java.sql.Date(afterItem.getUpdated()
						.getTime()));
				stmt.setString(4, afterItemId);

				ResultSet rs = stmt.executeQuery();

				LinkedList<NodeItem> results = new LinkedList<NodeItem>();

				while (rs.next()) {
					results.push(new NodeItemImpl(rs.getString(1), rs
							.getString(2), rs.getTimestamp(3), rs.getString(4)));
				}

				return new ClosableIteratorImpl<NodeItem>(results.iterator());
			}
		} catch (SQLException e) {
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
	public NodeItem getNodeItem(String nodeId, String nodeItemId)
			throws NodeStoreException {
		PreparedStatement stmt = null;

		try {
			stmt = conn.prepareStatement(SQL.SELECT_SINGLE_ITEM);

			stmt.setString(1, nodeId);
			stmt.setString(2, nodeItemId);

			ResultSet rs = stmt.executeQuery();

			if (rs.next()) {
				return new NodeItemImpl(rs.getString(1), rs.getString(2),
						rs.getTimestamp(3), rs.getString(4));
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
			stmt = conn.prepareStatement(SQL.ADD_ITEM);
			
			stmt.setString(1, item.getNodeId());
			stmt.setString(2, item.getId());
			stmt.setTimestamp(3, new Timestamp(item.getUpdated().getTime()));
			stmt.setString(4, item.getPayload());
			
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
			stmt = conn.prepareStatement(SQL.UPDATE_ITEM);
			
			stmt.setTimestamp(1, new Timestamp(item.getUpdated().getTime()));
			stmt.setString(2, item.getPayload());
			stmt.setString(3, item.getNodeId());
			stmt.setString(4, item.getId());
			
			int rows = stmt.executeUpdate();
			
			if(rows != 1) {
				throw new ItemNotFoundException("No records affected when updating an item");
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public void deleteNodeItemById(String nodeId, String nodeItemId) throws NodeStoreException {
		PreparedStatement stmt = null;
		
		try {
			stmt = conn.prepareStatement(SQL.DELETE_ITEM);
			
			stmt.setString(1, nodeId);
			stmt.setString(2, nodeItemId);
			
			int rows = stmt.executeUpdate();
			
			if(rows != 1) {
				throw new ItemNotFoundException("No records affected when deleting an item");
			}
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			close(stmt); // Will implicitly close the resultset if required
		}
	}

	@Override
	public Transaction beginTransaction() throws NodeStoreException {
		if(transactionHasBeenRolledBack) {
			throw new IllegalStateException("The transaction has already been rolled back");
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
				stmt.close();
			} catch (SQLException e) {
				logger.error(
						"SQLException thrown while trying to close a statement",
						e);
			}
		}
	}
	
	public class JDBCTransaction implements Transaction {
		private JDBCNodeStore store;
		private boolean closed;
		
		private JDBCTransaction(final JDBCNodeStore store) throws SQLException {
			this.store = store;
			closed = false;
			
			if(store.transactionStack.isEmpty()) {
				store.conn.setAutoCommit(false);
			}
			
			store.transactionStack.push(this);
		}
		
		@Override
		public void commit() throws NodeStoreException {
			if(closed) {
				throw new IllegalStateException("Commit called on transaction that is already closed");
			}
			if(!isLatestTransaction()) {
				throw new IllegalStateException("Commit called on transaction other than the innermost transaction");
			}
			if(store.transactionHasBeenRolledBack) {
				throw new IllegalStateException("Commit called after inner transaction has already been rolled back");
			}
			
			store.transactionStack.pop();
			closed = true;
			
			try {
				if(store.transactionStack.isEmpty()) {
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
			if(closed) {
				return;	// Do nothing nicely and silently
			}
			
			if(!isLatestTransaction()) {
				throw new IllegalStateException("Close called on transaction other than the innermost transaction");
			}
			
			store.transactionStack.pop();
			closed = true;
			store.transactionHasBeenRolledBack = true;
					
			try {
				if(store.transactionStack.isEmpty()) {
					store.conn.rollback();
					store.conn.setAutoCommit(true);
					store.transactionHasBeenRolledBack = false;
				}
			} catch (SQLException e) {
				throw new NodeStoreException(e);
			}
		}
		
		private boolean isLatestTransaction() {
			return(store.transactionStack.peek() == this);
		}
	}
	
	private class SQL {
		private static final String ADD_NODE = "INSERT INTO \"nodes\" ( \"node\" ) VALUES ( ? )";

		private static final String ADD_CONF = "INSERT INTO \"node_config\" ( \"node\", \"key\", \"value\", \"updated\" )"
				+ " VALUES ( ?, ?, ?, now() )";

		private static final String UPDATE_CONF = "UPDATE \"node_config\" SET \"value\" = ?, \"updated\" = now()"
				+ " WHERE \"node\" = ? AND \"key\" = ?";

		private static final String GET_SINGLE_NODE_CONF_VALUE = "SELECT \"value\" FROM \"node_config\""
				+ " WHERE \"node\" = ? AND \"key\" = ?";

		private static final String SELECT_AFFILIATION = "SELECT \"affiliation\" FROM \"affiliations\""
				+ " WHERE \"node\" = ? AND \"user\" = ?";

		private static final String ADD_AFFILIATION = "INSERT INTO \"affiliations\" ( \"node\", \"user\", \"affiliation\", \"updated\" )"
				+ " VALUES ( ?, ?, ?, now() )";

		private static final String UPDATE_AFFILIATION = "UPDATE \"affiliations\""
				+ " SET \"affiliation\" = ?, \"updated\" = now()"
				+ " WHERE \"node\" = ? AND \"user\" = ?";

		private static final String DELETE_AFFILIATION = "DELETE FROM \"affiliations\" WHERE \"node\" = ? AND \"user\" = ?;";

		private static final String SELECT_SUBSCRIPTIONS_FOR_USER = "SELECT \"node\", \"user\", \"listener\", \"subscription\""
				+ " FROM \"subscriptions\" WHERE \"user\" = ? OR \"listener\" = ?";

		private static final String ADD_SUBSCRIPTION = "INSERT INTO \"subscriptions\" ( \"node\", \"user\", \"listener\", \"subscription\", \"updated\" )"
				+ " VALUES ( ?, ?, ?, ?, now() )";

		private static final String UPDATE_SUBSCRIPTION = "UPDATE \"subscriptions\""
				+ " SET \"subscription\" = ?, \"updated\" = now()"
				+ " WHERE \"node\" = ? AND \"user\" = ? AND \"listener\" = ?";

		private static final String DELETE_SUBSCRIPTION = "DELETE FROM \"subscriptions\" WHERE \"node\" = ? AND \"user\" = ? AND \"listener\" = ?";

		private static final String NODE_EXISTS = "SELECT \"node\" FROM \"nodes\" WHERE \"node\" = ?";

		private static final String SELECT_SINGLE_ITEM = "SELECT \"node\", \"id\", \"updated\", \"xml\""
				+ " FROM \"items\" WHERE \"node\" = ? AND \"id\" = ?";

		private static final String SELECT_ITEMS_FOR_NODE = "SELECT \"node\", \"id\", \"updated\", \"xml\""
				+ " FROM \"items\" WHERE \"node\" = ? ORDER BY \"updated\" DESC, \"id\" ASC";

		private static final String SELECT_ITEMS_FOR_NODE_AFTER_DATE = "SELECT \"node\", \"id\", \"updated\", \"xml\""
				+ " FROM \"items\" WHERE \"node\" = ? AND ( \"updated\" > ? OR ( \"updated\" = ? AND \"id\" > ? ) )"
				+ " ORDER BY \"updated\" ASC, \"id\" DESC";

		private static final String ADD_ITEM = "INSERT INTO \"items\" ( \"node\", \"id\", \"updated\", \"xml\" )"
				+ " VALUES ( ?, ?, ?, ? )";

		private static final String UPDATE_ITEM = "UPDATE \"items\" SET \"updated\" = ?, \"xml\" = ?"
				+ " WHERE \"node\" = ? AND \"id\" = ?";

		private static final String DELETE_ITEM = "DELETE FROM \"items\" WHERE \"node\" = ? AND \"id\" = ?;";
	}
}
