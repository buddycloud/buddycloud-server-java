package org.buddycloud.channelserver.channel;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.concurrent.LinkedBlockingQueue;

import org.buddycloud.channelserver.Configuration;
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
        consumer = new InQueueConsumer(outQueue, Configuration.getInstance(), inQueue, channelManagerFactory, null, null);
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
                        e.printStackTrace();
                    } catch (IOException e) {
                        e.printStackTrace();
                    } catch (ClassNotFoundException e) {
                        e.printStackTrace();
                    }
                    return null;
            }
        };
        
        try {
            IQTestHandler.readConf();
            channelManagerFactory = new ChannelManagerFactoryImpl(nsFactory);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
    
}
