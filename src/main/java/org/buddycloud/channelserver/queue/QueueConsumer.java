package org.buddycloud.channelserver.queue;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.log4j.Logger;
import org.xmpp.packet.Packet;

public abstract class QueueConsumer {

    private static final Logger LOGGER = Logger.getLogger(QueueConsumer.class);
    
    private ExecutorService executorService = Executors.newFixedThreadPool(1);
    private final BlockingQueue<Packet> queue;
    
    public QueueConsumer(BlockingQueue<Packet> queue) {
        this.queue = queue;
    }

    public void start() {
        executorService.submit(new Runnable() {
            @Override
            public void run() {
                while (true) {
                    try {
                        Packet packet = queue.take();
                        consume(packet);
                    } catch (InterruptedException e) {
                        LOGGER.error(e);
                    }
                }
            }
        });
    }
    
    protected abstract void consume(Packet p);
    
}
