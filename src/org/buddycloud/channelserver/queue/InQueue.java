package org.buddycloud.channelserver.queue;

import java.io.StringReader;
import java.util.Properties;
import java.util.concurrent.LinkedBlockingQueue;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.DataStore;
import org.buddycloud.channelserver.packetHander.IQ.IQHandler;
import org.buddycloud.channelserver.packetHander.Message.MessageHandler;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;

public class InQueue {

    Thread[] consumers;
    
    protected LinkedBlockingQueue<String> queue = new LinkedBlockingQueue<String>();
    
    private Properties conf;
    
    public InQueue(AOutQueue queue, Properties conf) {
        
        this.consumers = new Thread[1];
        this.conf = conf;
        
        for (int i = 0; i < consumers.length; i++) {
            this.consumers[i] = new Thread(new Consumer(queue));
            this.consumers[i].start();
        }
        
    }

    public void put(String payload) {
        try {
            this.queue.put(payload);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
    
    private class Consumer implements Runnable {
        
        private Logger LOGGER = Logger.getLogger(Consumer.class);
        private AOutQueue outQueue;
        private IQHandler iqHandler;
        private MessageHandler messageHandler;
        
        private DataStore dataStore;
        
        public Consumer(AOutQueue queue) {
            
            if(queue == null) {
                this.outQueue = new OutQueue();
            } else {
                this.outQueue = queue;
            }
            
            this.dataStore = new DataStore(conf);
            
            this.iqHandler      = new IQHandler(this.outQueue, conf, dataStore);
            this.messageHandler = new MessageHandler(this.outQueue, InQueue.this, conf, dataStore);
        }
        
        public void run() {
            
            SAXReader xmlReader = new SAXReader();
            Element entry = null;
            Long start;
            
            while (true) {
            
                try {
                    
                    String packetPayload = queue.take();
                    
                    start = System.currentTimeMillis();
                    
                    LOGGER.debug("Received payload: '" + packetPayload + "'.");
                    
                    String subStr = packetPayload.substring(0, 3);
                    if( subStr.equals("<iq") ) {
                        
                        entry = xmlReader.read(new StringReader(packetPayload)).getRootElement();
                        this.iqHandler.ingestIQ(new IQ(entry));
                        
                    } else if( subStr.equals("<me") ) {
                        
                        entry = xmlReader.read(new StringReader(packetPayload)).getRootElement();
                        this.messageHandler.ingestMessage(new Message(entry));
                        
                    } else {
                        LOGGER.info("Not handling following stanzas yet: '" + packetPayload + "'.");
                    }
                    
                    LOGGER.debug("Payload handled in '" + Long.toString((System.currentTimeMillis() - start)) + "' milliseconds.");
                    
                } catch (InterruptedException e) {
                    LOGGER.debug("InterruptedException: " + e.getMessage(), e);
                } catch (DocumentException e) {
                    e.printStackTrace();
                    LOGGER.debug("DocumentException : " + e.getMessage(), e);
                } catch (Exception e) {
                    LOGGER.debug("Exception: " + e.getMessage(), e);
                }
            }
        }
    }
    
}
