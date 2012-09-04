package org.buddycloud.channelserver.db.jdbc;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.NodeStoreException;
import org.buddycloud.channelserver.node.NodeRef;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.affiliation.NodeAffiliation;
import org.buddycloud.channelserver.pubsub.model.Node;
import org.buddycloud.channelserver.pubsub.subscription.NodeSubscription;
import org.xmpp.packet.JID;

public class JDBCNodeStore implements NodeStore {

	private static final String SQL_ADD_NODE = "INSERT INTO nodes ( node ) VALUES ( ? );";

	private static final String SQL_ADD_CONF = "INSERT INTO node_config ( node, \"key\", \"value\", updated ) VALUES ( ?, ?, ?, now() );";
	private static final String SQL_UPDATE_CONF = "UPDATE node_config SET \"value\" = ?, updated = now() WHERE node = ? AND \"key\" = ?;";
	
	private final Connection conn;
	
	private static PreparedStatement addNodeStatement;
	private static PreparedStatement addConfStatement;
	private static PreparedStatement updateConfStatement;
	
	/**
	 * Create a new node store connection backed by the given JDBC {@link Connection}.
	 * @param conn the connection to the backing database.
	 */
	public JDBCNodeStore(final Connection conn) throws NodeStoreException {
		this.conn = conn;
		
		try {
			addNodeStatement = conn.prepareStatement(SQL_ADD_NODE);
			addConfStatement = conn.prepareStatement(SQL_ADD_CONF);
			updateConfStatement = conn.prepareStatement(SQL_UPDATE_CONF);
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		}
	}

	@Override
	public void createNode(JID owner, String nodeId,
			Map<String, String> nodeConf) throws NodeStoreException {
		try {
			beginTransaction();
			
			// Store node
			addConfStatement.clearParameters();
			addConfStatement.setString(1, nodeId);
			addConfStatement.executeUpdate();
			
			// Store the config (if there is any)
			if(nodeConf != null) {
				for(Entry<String,String> entry : nodeConf.entrySet()) {
					setNodeConf(nodeId, entry.getKey(), entry.getValue());
				}
			}
			
			setUserAffiliation(nodeId, owner, Affiliations.owner);
			
			commitTransaction();
		} catch (SQLException e) {
			throw new NodeStoreException(e);
		} finally {
			closeTransaction();
		}
		
	}

	@Override
	public void setNodeConf(String nodeId, String key, String value) {
		try {
			beginTransaction();
			
			updateConfStatement.clearParameters();
			updateConfStatement.setString(1, value);
			updateConfStatement.setString(2, nodeId);
			updateConfStatement.setString(3, key);
			
			if(updateConfStatement.executeUpdate() == 0) {	// If the update didn't update any rows
				addConfStatement.clearParameters();
				addConfStatement.setString(1, nodeId);
				addConfStatement.setString(2, key);
				addConfStatement.setString(3, value);
				
				addConfStatement.executeUpdate();
			}
			
			commitTransaction();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} finally {
			closeTransaction();
		}
	}

	@Override
	public Node fetchNode(String nodeId) throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean nodeExists(String nodeId) throws NodeStoreException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setUserAffiliation(String nodeId, JID user,
			Affiliations affiliation) throws NodeStoreException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void addUserSubscription(NodeSubscription subscription)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public NodeAffiliation getUserAfilliation(String nodeId, JID user)
			throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<NodeSubscription> getUserSubscriptions(String nodeId,
			JID user) throws NodeStoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void beginTransaction() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void commitTransaction() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void closeTransaction() {
		// TODO Auto-generated method stub
		
	}
}
