package org.buddycloud.channelserver.db.jdbc.dialect;

import org.buddycloud.channelserver.db.jdbc.JDBCNodeStore.NodeStoreSQLDialect;

public class Sql92NodeStoreDialect implements NodeStoreSQLDialect {
	private static final String INSERT_NODE = "INSERT INTO \"nodes\" ( \"node\" ) VALUES ( ? )";

	private static final String INSERT_CONF = "INSERT INTO \"node_config\" ( \"node\", \"key\", \"value\", \"updated\" )"
			+ " VALUES ( ?, ?, ?, now() )";

	private static final String DELETE_CONF_FROM_NODE = "DELETE FROM \"node_config\" WHERE \"node\" = ?";

	private static final String UPDATE_CONF = "UPDATE \"node_config\" SET \"value\" = ?, \"updated\" = now()"
			+ " WHERE \"node\" = ? AND \"key\" = ?";

	private static final String SELECT_SINGLE_NODE_CONF_VALUE = "SELECT \"value\" FROM \"node_config\""
			+ " WHERE \"node\" = ? AND \"key\" = ?";

	private static final String SELECT_NODE_CONF = "SELECT \"key\", \"value\" FROM \"node_config\""
			+ " WHERE \"node\" = ? ORDER BY \"key\" ASC";

	private static final String SELECT_AFFILIATION = "SELECT \"affiliation\", \"updated\" FROM \"affiliations\""
			+ " WHERE \"node\" = ? AND \"user\" = ?";

	private static final String SELECT_AFFILIATIONS_FOR_USER = "SELECT \"node\", \"user\", \"affiliation\", \"updated\""
			+ " FROM \"affiliations\" WHERE \"user\" = ? ORDER BY \"updated\" DESC";

	private static final String SELECT_AFFILIATION_CHANGES = ""
			+ "SELECT \"node\", \"user\", \"affiliation\", \"updated\" FROM \"affiliations\" "
			+ "WHERE \"updated\" >= ? AND \"updated\" <= ? AND \"node\" IN "
			+ "(SELECT \"subscriptions\".\"node\" FROM \"subscriptions\", \"affiliations\" "
			+ "WHERE \"subscriptions\".\"user\" = ? AND "
			+ "\"subscriptions\".\"subscription\" = 'subscribed' AND "
			+ "\"affiliations\".\"node\" = \"subscriptions\".\"node\" "
			+ "AND \"subscriptions\".\"user\" = \"affiliations\".\"user\" "
			+ "AND \"affiliations\".\"affiliation\" != 'banned'  "
			+ "AND \"affiliations\".\"affiliation\" != 'outcast') "
			+ "ORDER BY \"updated\" ASC;";

	private static final String SELECT_AFFILIATIONS_FOR_USER_AFTER_NODE_ID = "SELECT \"node\", \"user\", \"affiliation\", \"updated\""
			+ " FROM \"affiliations\" WHERE \"user\" = ? AND "
			+ "\"updated\" < (SELECT \"updated\" FROM \"affiliations\" WHERE \"user\" = ? AND \"node\" = ?) "
			+ "ORDER BY \"updated\" DESC LIMIT ?";

	private static final String COUNT_AFFILIATIONS_FOR_USER = "SELECT COUNT(*)"
			+ " FROM \"affiliations\" WHERE \"user\" = ?";

	private static final String SELECT_AFFILIATIONS_FOR_NODE = "SELECT \"node\", \"user\", \"affiliation\", \"updated\""
			+ " FROM \"affiliations\" WHERE \"node\" = ? ORDER BY \"updated\" DESC";

	private static final String SELECT_AFFILIATIONS_FOR_NODE_AFTER_JID = "SELECT \"node\", \"user\", \"affiliation\", \"updated\""
			+ " FROM \"affiliations\" WHERE \"node\" = ? AND "
			+ "\"updated\" < (SELECT \"updated\" FROM \"affiliations\" WHERE \"node\" = ? AND \"user\" = ?) "
			+ "ORDER BY \"updated\" DESC LIMIT ?";

	private static final String COUNT_AFFILIATIONS_FOR_NODE = "SELECT COUNT(*)"
			+ " FROM \"affiliations\" WHERE \"node\" = ?";

	private static final String INSERT_AFFILIATION = "INSERT INTO \"affiliations\" ( \"node\", \"user\", \"affiliation\", \"updated\" )"
			+ " VALUES ( ?, ?, ?, now() )";

	private static final String UPDATE_AFFILIATION = "UPDATE \"affiliations\""
			+ " SET \"affiliation\" = ?, \"updated\" = now()"
			+ " WHERE \"node\" = ? AND \"user\" = ?";

	private static final String DELETE_AFFILIATION = "DELETE FROM \"affiliations\" WHERE \"node\" = ? AND \"user\" = ?;";

	private static final String SELECT_SUBSCRIPTION = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"node\" = ? AND (\"user\" = ? OR \"listener\" = ? ) ORDER BY \"updated\" DESC";

	private static final String SELECT_SUBSCRIPTIONS_FOR_USER = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"user\" = ? OR \"listener\" = ? ORDER BY \"updated\" DESC";

	private static final String SELECT_SUBSCRIPTIONS_FOR_USER_AFTER_NODE = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE (\"user\" = ? OR \"listener\" = ?) AND "
			+ "\"updated\" < (SELECT \"updated\" FROM \"affiliations\" WHERE \"node\" = ? AND \"user\" = ?) "
			+ "ORDER BY \"updated\" DESC LIMIT ?";

	private static final String SELECT_SUBSCRIPTION_CHANGES = ""
			+ "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\" "
			+ "FROM \"subscriptions\" "
			+ "WHERE \"updated\" >= ? AND \"updated\" <= ? AND \"node\" IN "
			+ "(SELECT \"subscriptions\".\"node\" FROM \"subscriptions\", \"affiliations\" "
			+ "WHERE \"subscriptions\".\"user\" = ? AND "
			+ "\"subscriptions\".\"subscription\" = 'subscribed' AND "
			+ "\"affiliations\".\"node\" = \"subscriptions\".\"node\" "
			+ "AND \"subscriptions\".\"user\" = \"affiliations\".\"user\" "
			+ "AND \"affiliations\".\"affiliation\" != 'banned'  "
			+ "AND \"affiliations\".\"affiliation\" != 'outcast') "
			+ "ORDER BY \"updated\" ASC;";

	private static final String SELECT_SUBSCRIPTIONS_FOR_NODE = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"node\" = ? ORDER BY \"updated\" DESC";

	private static final String SELECT_SUBSCRIPTIONS_FOR_NODE_AFTER_JID = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"node\" = ? AND "
			+ "\"updated\" < (SELECT \"updated\" FROM \"subscriptions\" WHERE \"node\" = ? AND \"user\" = ?) "
			+ "ORDER BY \"updated\" DESC LIMIT ?";

	private static final String INSERT_SUBSCRIPTION = "INSERT INTO \"subscriptions\" ( \"node\", \"user\", \"listener\", \"subscription\", \"updated\" )"
			+ " VALUES ( ?, ?, ?, ?, now() )";

	private static final String UPDATE_SUBSCRIPTION = "UPDATE \"subscriptions\""
			+ " SET \"subscription\" = ?, \"updated\" = now(), \"listener\" = ?"
			+ " WHERE \"node\" = ? AND \"user\" = ?";

	private static final String DELETE_SUBSCRIPTION = "DELETE FROM \"subscriptions\" WHERE \"node\" = ? AND \"user\" = ?";

	private static final String NODE_EXISTS = "SELECT \"node\" FROM \"nodes\" WHERE \"node\" = ?";

	private static final String SELECT_SINGLE_ITEM = "SELECT \"node\", \"id\", \"updated\", \"xml\""
			+ " FROM \"items\" WHERE \"node\" = ? AND \"id\" = ?";

	private static final String SELECT_ITEMS_FOR_NODE = "SELECT \"node\", \"id\", \"updated\", \"xml\""
			+ " FROM \"items\" WHERE \"node\" = ? ORDER BY \"updated\" DESC, \"id\" ASC";

	private static final String SELECT_ITEMS_FOR_NODE_AFTER_DATE = "SELECT \"node\", \"id\", \"updated\", \"xml\""
			+ " FROM \"items\" WHERE \"node\" = ? AND ( \"updated\" > ? OR ( \"updated\" = ? AND \"id\" > ? ) )"
			+ " ORDER BY \"updated\" ASC, \"id\" DESC";

	private static final String SELECT_ITEMS_FOR_NODE_BEFORE_DATE = "SELECT \"node\", \"id\", \"updated\", \"xml\""
			+ " FROM \"items\" WHERE \"node\" = ? AND ( \"updated\" < ? OR ( \"updated\" = ? AND \"id\" < ? ) )"
			+ " ORDER BY \"updated\" DESC, \"id\" ASC";
	
	private static final String SELECT_ITEMS_FOR_USER_BETWEEN_DATES = ""
	        + "SELECT \"node\", \"id\", \"updated\", \"xml\""
			+ " FROM \"items\" "
			+ "WHERE \"updated\" >= ? AND \"updated\" <= ? AND \"node\" IN "
			+ "(SELECT \"subscriptions\".\"node\" FROM \"subscriptions\", \"affiliations\" "
			+ "WHERE \"subscriptions\".\"user\" = ? AND "
			+ "\"subscriptions\".\"subscription\" = 'subscribed' AND "
			+ "\"affiliations\".\"node\" = \"subscriptions\".\"node\" "
			+ "AND \"subscriptions\".\"user\" = \"affiliations\".\"user\" "
			+ "AND \"affiliations\".\"affiliation\" != 'banned'  "
			+ "AND \"affiliations\".\"affiliation\" != 'outcast') "
			+ "ORDER BY \"updated\" ASC;";

	private static final String COUNT_ITEMS_FOR_NODE = "SELECT COUNT(*)"
			+ " FROM \"items\" WHERE \"node\" = ?";

	private static final String COUNT_SUBSCRIPTIONS_FOR_NODE = "SELECT COUNT(*) "
			+ "FROM \"subscriptions\" WHERE \"node\" = ?;";

	private static final String COUNT_ITEMS_FOR_JID = "SELECT COUNT(*)"
			+ " FROM \"subscriptions\" WHERE \"user\" = ?";

	private static final String INSERT_ITEM = "INSERT INTO \"items\" ( \"node\", \"id\", \"updated\", \"xml\" )"
			+ " VALUES ( ?, ?, ?, ? )";

	private static final String UPDATE_ITEM = "UPDATE \"items\" SET \"updated\" = ?, \"xml\" = ?"
			+ " WHERE \"node\" = ? AND \"id\" = ?";

	private static final String DELETE_ITEM = "DELETE FROM \"items\" WHERE \"node\" = ? AND \"id\" = ?;";

	private static final String SELECT_SUBSCRIPTION_LISTENERS_FOR_NODE = "SELECT DISTINCT \"listener\", \"node\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"node\" = ? ORDER BY \"updated\"";

	private static final String DELETE_NODE = "DELETE FROM \"nodes\" WHERE \"node\" = ?;";

	private static final String SELECT_NODE_LIST = "SELECT \"node\" FROM \"nodes\";";

	private static final String DELETE_ITEMS = "DELETE FROM \"items\" WHERE \"node\" = ?;";

	@Override
	public String insertNode() {
		return INSERT_NODE;
	}

	@Override
	public String insertConf() {
		return INSERT_CONF;
	}

	@Override
	public String deleteConfFromNode() {
		return DELETE_CONF_FROM_NODE;
	}

	@Override
	public String updateNodeConf() {
		return UPDATE_CONF;
	}

	@Override
	public String selectSingleNodeConfValue() {
		return SELECT_SINGLE_NODE_CONF_VALUE;
	}

	@Override
	public String selectNodeConf() {
		return SELECT_NODE_CONF;
	}

	@Override
	public String selectAffiliation() {
		return SELECT_AFFILIATION;
	}

	@Override
	public String selectAffiliationsForUser() {
		return SELECT_AFFILIATIONS_FOR_USER;
	}

	@Override
	public String selectAffiliationChanges() {
		return SELECT_AFFILIATION_CHANGES;
	}

	@Override
	public String selectAffiliationsForUserAfterNodeId() {
		return SELECT_AFFILIATIONS_FOR_USER_AFTER_NODE_ID;
	}

	@Override
	public String countUserAffiliations() {
		return COUNT_AFFILIATIONS_FOR_USER;
	}

	@Override
	public String selectAffiliationsForNode() {
		return SELECT_AFFILIATIONS_FOR_NODE;
	}

	@Override
	public String selectAffiliationsForNodeAfterJid() {
		return SELECT_AFFILIATIONS_FOR_NODE_AFTER_JID;
	}

	@Override
	public String countNodeAffiliations() {
		return COUNT_AFFILIATIONS_FOR_NODE;
	}

	@Override
	public String insertAffiliation() {
		return INSERT_AFFILIATION;
	}

	@Override
	public String updateAffiliation() {
		return UPDATE_AFFILIATION;
	}

	@Override
	public String deleteAffiliation() {
		return DELETE_AFFILIATION;
	}

	@Override
	public String selectSubscription() {
		return SELECT_SUBSCRIPTION;
	}

	@Override
	public String selectSubscriptionsForUser() {
		return SELECT_SUBSCRIPTIONS_FOR_USER;
	}

	@Override
	public String selectSubscriptionsForUserAfterNode() {
		return SELECT_SUBSCRIPTIONS_FOR_USER_AFTER_NODE;
	}

	@Override
	public String getSubscriptionChanges() {
		return SELECT_SUBSCRIPTION_CHANGES;
	}

	@Override
	public String selectSubscriptionsForNode() {
		return SELECT_SUBSCRIPTIONS_FOR_NODE;
	}

	@Override
	public String selectSubscriptionsForNodeAfterJid() {
		return SELECT_SUBSCRIPTIONS_FOR_NODE_AFTER_JID;
	}

	public String countSubscriptionsForJid() {
		return COUNT_ITEMS_FOR_JID;
	}

	@Override
	public String countSubscriptionsForNode() {
		return COUNT_SUBSCRIPTIONS_FOR_NODE;
	}

	@Override
	public String insertSubscription() {
		return INSERT_SUBSCRIPTION;
	}

	@Override
	public String updateSubscription() {
		return UPDATE_SUBSCRIPTION;
	}

	@Override
	public String deleteSubscription() {
		return DELETE_SUBSCRIPTION;
	}

	@Override
	public String nodeExists() {
		return NODE_EXISTS;
	}

	@Override
	public String selectSingleItem() {
		return SELECT_SINGLE_ITEM;
	}

	@Override
	public String selectItemsForNode() {
		return SELECT_ITEMS_FOR_NODE;
	}

	@Override
	public String selectItemsForNodeAfterDate() {
		return SELECT_ITEMS_FOR_NODE_AFTER_DATE;
	}

	@Override
	public String selectItemsForNodeBeforeDate() {
		return SELECT_ITEMS_FOR_NODE_BEFORE_DATE;
	}
	
	@Override
	public String selectItemsForUsersNodesBetweenDates() {
		return SELECT_ITEMS_FOR_USER_BETWEEN_DATES;
	}

	@Override
	public String countItemsForNode() {
		return COUNT_ITEMS_FOR_NODE;
	}

	@Override
	public String insertItem() {
		return INSERT_ITEM;
	}

	@Override
	public String updateItem() {
		return UPDATE_ITEM;
	}

	@Override
	public String deleteItem() {
		return DELETE_ITEM;
	}

	@Override
	public String selectSubscriptionListenersForNode() {
		return SELECT_SUBSCRIPTION_LISTENERS_FOR_NODE;
	}

	@Override
	public String deleteNode() {
		return DELETE_NODE;
	}

	@Override
	public String deleteItems() {
		// TODO Auto-generated method stub
		return DELETE_ITEMS;
	}

	@Override
	public String selectNodeList() {
		// TODO Auto-generated method stub
		return SELECT_NODE_LIST;
	}
}