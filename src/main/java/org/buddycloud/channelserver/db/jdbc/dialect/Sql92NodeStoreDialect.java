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
			+ " FROM \"affiliations\" WHERE \"user\" = ? ORDER BY \"updated\" ASC";

	private static final String SELECT_NODE_OWNERS = "SELECT \"user\" FROM \"affiliations\" WHERE \"node\" = ? AND \"affiliation\" = 'owner';";
	
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
			+ "\"updated\" > (SELECT \"updated\" FROM \"affiliations\" WHERE \"user\" = ? AND \"node\" = ?) "
			+ "ORDER BY \"updated\" ASC LIMIT ?";

	private static final String COUNT_AFFILIATIONS_FOR_USER = "SELECT COUNT(*)"
			+ " FROM \"affiliations\" WHERE \"user\" = ?";

	private static final String SELECT_AFFILIATIONS_FOR_NODE = "SELECT \"node\", \"user\", \"affiliation\", \"updated\""
			+ " FROM \"affiliations\" WHERE \"node\" = ? AND \"affiliation\" != 'outcast' ORDER BY \"updated\" ASC";

	private static final String SELECT_AFFILIATIONS_TO_NODE_FOR_OWNER = "SELECT \"node\", \"user\", \"affiliation\", \"updated\""
			+ " FROM \"affiliations\" WHERE \"node\" = ? ORDER BY \"updated\" ASC";
	
	private static final String SELECT_AFFILIATIONS_FOR_NODE_AFTER_JID = "SELECT \"node\", \"user\", \"affiliation\", \"updated\""
			+ " FROM \"affiliations\" WHERE \"node\" = ? AND \"affiliation\" != 'outcast' AND "
			+ "\"updated\" > (SELECT \"updated\" FROM \"affiliations\" WHERE \"node\" = ? AND \"user\" = ?) "
			+ "ORDER BY \"updated\" ASC LIMIT ?";
	
	private static final String SELECT_AFFILIATIONS_TO_NODE_FOR_OWNER_AFTER_JID = "SELECT \"node\", \"user\", \"affiliation\", \"updated\""
			+ " FROM \"affiliations\" WHERE \"node\" = ? AND "
			+ "\"updated\" > (SELECT \"updated\" FROM \"affiliations\" WHERE \"node\" = ? AND \"user\" = ?) "
			+ "ORDER BY \"updated\" ASC LIMIT ?";

	private static final String COUNT_AFFILIATIONS_FOR_NODE = "SELECT COUNT(*)"
			+ " FROM \"affiliations\" WHERE \"node\" = ? AND \"affiliation\" != 'outcast';";
	
	private static final String COUNT_AFFILIATIONS_TO_NODE_FOR_OWNER = "SELECT COUNT(*)"
			+ " FROM \"affiliations\" WHERE \"node\" = ?";

	private static final String INSERT_AFFILIATION = "INSERT INTO \"affiliations\" ( \"node\", \"user\", \"affiliation\", \"updated\" )"
			+ " VALUES ( ?, ?, ?, now() )";

	private static final String UPDATE_AFFILIATION = "UPDATE \"affiliations\""
			+ " SET \"affiliation\" = ?, \"updated\" = now()"
			+ " WHERE \"node\" = ? AND \"user\" = ?";

	private static final String DELETE_AFFILIATION = "DELETE FROM \"affiliations\" WHERE \"node\" = ? AND \"user\" = ?;";

	private static final String SELECT_SUBSCRIPTION = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"node\" = ? AND (\"user\" = ? OR \"listener\" = ? ) ORDER BY \"updated\" ASC";

	private static final String SELECT_SUBSCRIPTIONS_FOR_USER = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"user\" = ? OR \"listener\" = ? ORDER BY \"updated\" ASC";

	private static final String SELECT_SUBSCRIPTIONS_FOR_USER_AFTER_NODE = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE (\"user\" = ? OR \"listener\" = ?) AND "
			+ "\"updated\" > (SELECT \"updated\" FROM \"affiliations\" WHERE \"node\" = ? AND \"user\" = ?) "
			+ "ORDER BY \"updated\" ASC LIMIT ?";

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

	private static final String SELECT_SUBSCRIPTIONS_FOR_NODE = "SELECT \"s\".\"node\", \"s\".\"user\", \"s\".\"listener\", \"s\".\"subscription\", \"s\".\"updated\""
			+ " FROM \"subscriptions\" AS \"s\", \"affiliations\" AS \"a\" "
			+ "WHERE \"s\".\"node\" = ? AND \"s\".\"node\" = \"a\".\"node\" "
			+ "AND \"s\".\"user\" = \"a\".\"user\" AND \"a\".\"affiliation\" != 'outcast' "
			+ "ORDER BY \"s\".\"updated\" ASC";

	private static final String SELECT_SUBSCRIPTIONS_TO_NODE_FOR_OWNER = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"node\" = ? ORDER BY \"updated\" ASC";
	
	private static final String SELECT_SUBSCRIPTIONS_FOR_NODE_AFTER_JID = "SELECT \"node\", \"user\", \"listener\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"node\" = ? AND "
			+ "\"updated\" > (SELECT \"updated\" FROM \"subscriptions\" WHERE \"node\" = ? AND \"user\" = ?) "
			+ "ORDER BY \"updated\" ASC LIMIT ?";

	private static final String INSERT_SUBSCRIPTION = "INSERT INTO \"subscriptions\" ( \"node\", \"user\", \"listener\", \"subscription\", \"updated\" )"
			+ " VALUES ( ?, ?, ?, ?, now() )";

	private static final String UPDATE_SUBSCRIPTION = "UPDATE \"subscriptions\""
			+ " SET \"subscription\" = ?, \"updated\" = now(), \"listener\" = ?"
			+ " WHERE \"node\" = ? AND \"user\" = ?";

	private static final String DELETE_SUBSCRIPTION = "DELETE FROM \"subscriptions\" WHERE \"node\" = ? AND \"user\" = ?";

	private static final String NODE_EXISTS = "SELECT \"node\" FROM \"nodes\" WHERE \"node\" = ?";

	private static final String SELECT_SINGLE_ITEM = "SELECT \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\""
			+ " FROM \"items\" WHERE \"node\" = ? AND \"id\" = ?";

	private static final String SELECT_ITEMS_FOR_NODE = "SELECT \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\""
			+ " FROM \"items\" WHERE \"node\" = ? ORDER BY \"updated\" DESC, \"id\" ASC";

	private static final String SELECT_ITEMS_FOR_NODE_AFTER_DATE = "SELECT \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\""
			+ " FROM \"items\" WHERE \"node\" = ? AND ( \"updated\" > ? OR ( \"updated\" = ? AND \"id\" > ? ) )"
			+ " ORDER BY \"updated\" ASC, \"id\" DESC";

	private static final String SELECT_ITEMS_FOR_NODE_BEFORE_DATE = "SELECT \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\""
			+ " FROM \"items\" WHERE \"node\" = ? AND ( \"updated\" < ? OR ( \"updated\" = ? AND \"id\" < ? ) )"
			+ " ORDER BY \"updated\" DESC, \"id\" ASC";
	
	private static final String SELECT_ITEMS_FOR_USER_BETWEEN_DATES = ""
	        + "SELECT \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\""
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

	private static final String SELECT_RECENT_ITEM_PARTS = ""
	    + "(SELECT \"id\", \"node\", \"xml\", \"updated\", \"in_reply_to\" "
		+ "FROM \"items\" "
		+ "WHERE \"node\" = ? "
        + "AND \"updated\" > ? "
		+ "%parentOnly% "
        + "ORDER BY \"updated\" DESC, \"id\" ASC LIMIT ?) ";

	private static final String SELECT_COUNT_RECENT_ITEM_PARTS = ""
		    + "(SELECT COUNT(\"id\") "
			+ "FROM \"items\" "
			+ "WHERE \"node\" = ? "
	        + "AND \"updated\" > ? "
			+ "%parentOnly% "
	        + "LIMIT ?)";

	private static final String COUNT_ITEMS_FOR_NODE = "SELECT COUNT(*)"
			+ " FROM \"items\" WHERE \"node\" = ?";

	private static final String SELECT_ITEM_REPLIES = ""
			+ "SELECT \"id\", \"node\", \"xml\", \"updated\", \"in_reply_to\" "
			+ "FROM \"items\" WHERE \"node\" = ? AND \"in_reply_to\" LIKE ? "
			+ "AND \"updated\" > ? ORDER BY \"updated\" DESC";
	
	private static final String SELECT_ITEM_THREAD = ""
			+ "SELECT \"id\", \"node\", \"xml\", \"updated\", \"in_reply_to\" "
			+ "FROM \"items\" WHERE \"node\" = ? "
			+ "AND (\"in_reply_to\" LIKE ? OR \"id\" = ?) "
			+ "AND \"updated\" > ? ORDER BY \"updated\" DESC";
	
	private static final String SELECT_COUNT_ITEM_REPLIES = ""
			+ "SELECT COUNT(\"id\") "
			+ "FROM \"items\" WHERE \"node\" = ? AND \"in_reply_to\" LIKE ? ";
	
	private static final String SELECT_COUNT_ITEM_THREAD = ""
			+ "SELECT COUNT(\"id\") "
			+ "FROM \"items\" WHERE \"node\" = ? "
			+ "AND (\"in_reply_to\" LIKE ? OR \"id\" = ?) ";
	
	private static final String COUNT_SUBSCRIPTIONS_FOR_NODE = "SELECT COUNT(*) "
			+ "FROM \"subscriptions\", \"affiliations\" WHERE "
			+ "\"subscriptions\".\"node\" = ? AND \"affiliations\".\"node\" = \"subscriptions\".\"node\" "
			+ "AND \"affiliations\".\"user\" = \"subscriptions\".\"user\" "
			+ "AND \"affiliations\".\"affiliation\" != 'outcast';";
	
	private static final String COUNT_SUBSCRIPTIONS_TO_NODE_FOR_OWNER  = "SELECT COUNT(*) "
			+ "FROM \"subscriptions\" WHERE "
			+ "\"subscriptions\".\"node\" = ?;";

	private static final String COUNT_ITEMS_FOR_JID = "SELECT COUNT(*)"
			+ " FROM \"subscriptions\" WHERE \"user\" = ?";

	private static final String INSERT_ITEM = "INSERT INTO \"items\" ( \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\" )"
			+ " VALUES ( ?, ?, ?, ?, ? )";

	private static final String UPDATE_ITEM = "UPDATE \"items\" SET \"updated\" = ?, \"xml\" = ?"
			+ " WHERE \"node\" = ? AND \"id\" = ?";

	private static final String UPDATE_THREAD_PARENT = "UPDATE \"items\" SET \"updated\"= NOW() WHERE \"node\" = ? AND \"id\" = ?;";

	private static final String DELETE_ITEM = "DELETE FROM \"items\" WHERE \"node\" = ? AND \"id\" = ?;";

	private static final String SELECT_SUBSCRIPTION_LISTENERS_FOR_NODE = "SELECT DISTINCT ON (\"listener\") \"listener\", \"node\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" WHERE \"node\" = ? ORDER BY \"listener\", \"updated\"";

	private static final String SELECT_SUBSCRIPTION_LISTENERS = "SELECT DISTINCT ON (\"listener\") \"listener\", \"node\", \"subscription\", \"updated\""
			+ " FROM \"subscriptions\" ORDER BY \"listener\", \"updated\"";
	
	private static final String DELETE_NODE = "DELETE FROM \"nodes\" WHERE \"node\" = ?;";

	private static final String SELECT_NODE_LIST = "SELECT \"node\" FROM \"nodes\";";

	private static final String DELETE_ITEMS = "DELETE FROM \"items\" WHERE \"node\" = ?;";

	private static final String COUNT_ITEMS_FROM_LOCAL_NODES = ""
        + "SELECT COUNT(\"id\") FROM \"items\" "
		+ "WHERE \"node\" IN (SELECT \"node\" FROM \"node_config\" "
        + "WHERE \"key\" = ? AND \"value\" LIKE ? "
		+ "AND (\"node\" LIKE ? OR \"node\" LIKE ?))";

	private static final String SELECT_ITEMS_FROM_LOCAL_NODES_BEFORE_DATE = ""
		+ "SELECT \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\" "
		+ "FROM \"items\" WHERE \"updated\" < ? "
		+ "AND \"node\" IN (SELECT \"node\" FROM \"node_config\" WHERE \"key\" = ? AND \"value\" LIKE ? AND (\"node\" LIKE ? OR \"node\" LIKE ?)) "
		+ "ORDER BY \"updated\" DESC, \"id\" ASC LIMIT ?";

	private static final String SELECT_USER_ITEMS = "SELECT \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\"" +
			" FROM \"items\" WHERE (CAST(xpath('//atom:author/atom:name/text()', xmlparse(document \"xml\")," +
			" ARRAY[ARRAY['atom', 'http://www.w3.org/2005/Atom']]) AS TEXT[]))[1] = ?";

	private static final String DELETE_USER_ITEMS = "DELETE" +
			" FROM \"items\" WHERE (CAST(xpath('//atom:author/atom:name/text()', xmlparse(document \"xml\")," +
			" ARRAY[ARRAY['atom', 'http://www.w3.org/2005/Atom']]) AS TEXT[]))[1] = ?";

	private static final String DELETE_USER_AFFILIATIONS = "DELETE FROM \"affiliations\" WHERE \"user\" = ?";

	private static final String DELETE_USER_SUBSCRIPTIONS = "DELETE FROM \"subscriptions\" WHERE \"user\" = ?";
	
	private static final String SELECT_NODE_THREADS = 
			"SELECT \"node\", \"id\", \"updated\", \"xml\", \"in_reply_to\", " +
			"\"thread_id\", \"thread_updated\" FROM \"items\"," +
			  "(SELECT MAX(\"updated\") AS \"thread_updated\", \"thread_id\" FROM " +
			  "(SELECT \"updated\", " +
			    "(CASE WHEN (\"in_reply_to\" IS NULL) THEN \"id\" ELSE \"in_reply_to\" END) AS \"thread_id\" " +
			    "FROM \"items\" WHERE \"node\" = ?) AS \"_items\" " +
			  "GROUP BY \"thread_id\" " +
			  "HAVING MAX(\"updated\") < ? " +
			  "ORDER BY \"thread_updated\" DESC LIMIT ?) AS \"threads\" " +
			"WHERE \"in_reply_to\" = \"thread_id\" OR \"id\" = \"thread_id\" " +
			"ORDER BY \"thread_updated\" DESC, \"updated\"";

	private static final String COUNT_NODE_THREADS = "SELECT COUNT(DISTINCT \"thread_id\") " +
			"FROM (SELECT \"node\", (CASE WHEN (\"in_reply_to\" IS NULL) THEN \"id\" ELSE \"in_reply_to\" END) AS \"thread_id\" " +
			      "FROM \"items\" WHERE \"node\" = ?) AS \"_items\"";
	
	private static final String SELECT_USER_POST_RATING = "SELECT \"node\", \"id\", \"updated\", \"xml\" " +
			"FROM \"items\" WHERE " +
			"\"node\" = ? " +
			"AND \"xml\" LIKE ? " +
			"AND \"xml\" LIKE ? " +
			"AND \"xml\" LIKE '%<activity:verb>rated</activity:verb>%';";
	
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
	public String selectNodeOwners() {
		return SELECT_NODE_OWNERS;
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
	public String selectAffiliationsToNodeForOwner() {
		return SELECT_AFFILIATIONS_TO_NODE_FOR_OWNER;
	}

	@Override
	public String selectAffiliationsForNodeAfterJid() {
		return SELECT_AFFILIATIONS_FOR_NODE_AFTER_JID;
	}
	
	@Override
	public String selectAffiliationsToNodeForOwnerAfterJid() {
		return SELECT_AFFILIATIONS_TO_NODE_FOR_OWNER_AFTER_JID;
	}

	@Override
	public String countNodeAffiliations() {
		return COUNT_AFFILIATIONS_FOR_NODE;
	}
	
	@Override
	public String countNodeAffiliationsForOwner() {
		return COUNT_AFFILIATIONS_TO_NODE_FOR_OWNER;
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
	public String selectSubscriptionsToNodeForOwner() {
		return SELECT_SUBSCRIPTIONS_TO_NODE_FOR_OWNER;
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
	public String countSubscriptionsToNodeForOwner() {
		return COUNT_SUBSCRIPTIONS_TO_NODE_FOR_OWNER;
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
	public String selectItemReplies() {
		return SELECT_ITEM_REPLIES;
	}
	
	@Override
	public String selectCountItemReplies() {
		return SELECT_COUNT_ITEM_REPLIES;
	}
	
	@Override
	public String selectItemThread() {
		return SELECT_ITEM_THREAD;
	}
	
	@Override
	public String selectCountItemThread() {
		return SELECT_COUNT_ITEM_THREAD;
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
	public String updateThreadParent() {
		return UPDATE_THREAD_PARENT;
	}

	@Override
	public String selectSubscriptionListenersForNode() {
		return SELECT_SUBSCRIPTION_LISTENERS_FOR_NODE;
	}
	
	@Override
	public String selectSubscriptionListeners() {
		return SELECT_SUBSCRIPTION_LISTENERS;
	}

	@Override
	public String deleteNode() {
		return DELETE_NODE;
	}

	@Override
	public String deleteItems() {
		return DELETE_ITEMS;
	}

	@Override
	public String selectNodeList() {
		return SELECT_NODE_LIST;
	}

	@Override
	public String selectRecentItemParts() {
        return SELECT_RECENT_ITEM_PARTS;
	}
	
	@Override
	public String selectCountRecentItemParts() {
		return SELECT_COUNT_RECENT_ITEM_PARTS;
	}

	@Override
	public String selectItemsForLocalNodesBeforeDate() {
		return SELECT_ITEMS_FROM_LOCAL_NODES_BEFORE_DATE;
	}
	
	@Override
	public String selectUserRatingsForAPost() {
		return SELECT_USER_POST_RATING;
	}

	@Override
	public String countItemsForLocalNodes() {
		return COUNT_ITEMS_FROM_LOCAL_NODES;
	}

	@Override
	public String getUserItems() {
		return SELECT_USER_ITEMS;
	}

	@Override
	public String deleteUserItems() {
		return DELETE_USER_ITEMS;
	}

	@Override
	public String deleteUserAffiliations() {
		return DELETE_USER_AFFILIATIONS;
	}

	@Override
	public String deleteUserSubscriptions() {
		return DELETE_USER_SUBSCRIPTIONS;
	}
	
	@Override
	public String selectNodeThreads() {
		return SELECT_NODE_THREADS;
	}

	@Override
	public String countNodeThreads() {
		return COUNT_NODE_THREADS;
	}
}
