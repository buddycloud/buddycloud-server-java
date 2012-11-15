package org.buddycloud.channelserver.channel;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.db.NodeStore;
import org.buddycloud.channelserver.db.NodeStoreFactory;
import org.buddycloud.channelserver.db.jdbc.DatabaseTester;
import org.buddycloud.channelserver.db.jdbc.JDBCNodeStore;
import org.buddycloud.channelserver.db.jdbc.dialect.Sql92NodeStoreDialect;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.queue.InQueueConsumer;
import org.xmpp.packet.Packet;

public class TestHelper {
	LinkedBlockingQueue<Packet> outQueue;
	LinkedBlockingQueue<Packet> inQueue;
	InQueueConsumer consumer;
	
	ChannelManagerFactory channelManagerFactory;
	
	public TestHelper() throws FileNotFoundException, IOException {
		initialiseChannelManagerFactory();
		
        outQueue = new LinkedBlockingQueue<Packet>();
        inQueue = new LinkedBlockingQueue<Packet>();
        consumer = new InQueueConsumer(outQueue, IQTestHandler.readConf(), inQueue, channelManagerFactory, null);
        consumer.start();
	}
	
	public LinkedBlockingQueue<Packet> getOutQueue() {
		return outQueue;
	}
	
	public LinkedBlockingQueue<Packet> getInQueue() {
		return inQueue;
	}
	
	public InQueueConsumer getConsumer() {
		return consumer;
	}
	  
	public ChannelManagerFactory getChannelManagerFactory() {
		return channelManagerFactory;
	}
	
    private ChannelManagerFactory initialiseChannelManagerFactory() {
    	NodeStoreFactory nsFactory = new NodeStoreFactory() {
			
			@Override
			public NodeStore create() {
					try {
						return new JDBCNodeStore(new DatabaseTester().getConnection(), new Sql92NodeStoreDialect());
					} catch (SQLException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (ClassNotFoundException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					return null;
			}
		};
    	
		try {
			channelManagerFactory = new ChannelManagerFactoryImpl(IQTestHandler.readConf(), nsFactory);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
    }
	
}
