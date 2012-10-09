package org.buddycloud.channelserver.db;

import java.sql.SQLException;


import org.buddycloud.channelserver.Configuration;
import java.util.Properties;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.jdbc.JDBCNodeStore;
import org.buddycloud.channelserver.db.jdbc.JDBCNodeStore.NodeStoreSQLDialect;
import org.buddycloud.channelserver.db.jdbc.dialect.Sql92NodeStoreDialect;
import org.logicalcobwebs.proxool.ProxoolException;
import java.sql.Connection;

public class DefaultNodeStoreFactoryImpl implements NodeStoreFactory {

	private static final String CONFIGURATION_JDBC_DIALECT = "jdbc.dialect";

	
	private final Properties configuration;

	private final NodeStoreSQLDialect dialect;

	public DefaultNodeStoreFactoryImpl(final Properties configuration)
			throws NodeStoreException {
		this.configuration = configuration;

		String dialectClass = configuration.getProperty(
				CONFIGURATION_JDBC_DIALECT,
				Sql92NodeStoreDialect.class.getName());

		try {
			dialect = (NodeStoreSQLDialect) Class.forName(dialectClass)
					.newInstance();
		} catch (Exception e) {
			throw new NodeStoreException("Could not instantiate dialect class "
					+ dialectClass, e);
		}
	}

	@Override
	public NodeStore create() {

		Connection connection = null;
		try {
			connection = new JDBCConnectionFactory(null).getConnection();
			return new JDBCNodeStore(
					connection,
					new Sql92NodeStoreDialect());
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ProxoolException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

}
