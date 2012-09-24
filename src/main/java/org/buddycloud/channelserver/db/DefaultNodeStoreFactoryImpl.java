package org.buddycloud.channelserver.db;

import java.util.Properties;

import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.db.jdbc.JDBCNodeStore.NodeStoreSQLDialect;
import org.buddycloud.channelserver.db.jdbc.dialect.Sql92NodeStoreDialect;

public class DefaultNodeStoreFactoryImpl implements NodeStoreFactory {

	private static final String CONFIGURATION_JDBC_DIALECT = "jdbc.dialect";
	
	private final Properties configuration;
	
	private final NodeStoreSQLDialect dialect;
	
	public DefaultNodeStoreFactoryImpl(final Properties configuration) throws NodeStoreException {
		this.configuration = configuration;
		
		String dialectClass = configuration.getProperty(CONFIGURATION_JDBC_DIALECT, Sql92NodeStoreDialect.class.getName());
		
			try {
				dialect = (NodeStoreSQLDialect) Class.forName(dialectClass).newInstance();
			} catch (Exception e) {
				throw new NodeStoreException("Could not instantiate dialect class " + dialectClass, e);
			}
	}

	@Override
	public NodeStore create() {
		// TODO Auto-generated method stub
		return null;
	}

}
