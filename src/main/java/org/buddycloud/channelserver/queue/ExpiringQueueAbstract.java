package org.buddycloud.channelserver.queue;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;

abstract public class ExpiringQueueAbstract extends ConcurrentHashMap<Object, Object> {

	private static Logger logger = Logger.getLogger(ExpiringQueueAbstract.class);
	private ConcurrentHashMap<Long, ArrayList> addedTime = new ConcurrentHashMap<Long, ArrayList>();
	
	private static final int DEFAULT_TIMEOUT = 180000;
	private int timeout = 0;
	
	private Timer timer;

	public ExpiringQueueAbstract() {
		super();
		setTimeout(DEFAULT_TIMEOUT);
		
	}
	
	public ExpiringQueueAbstract(int timeout) {
		super();
		setTimeout(timeout);
	}
	
	/**
	 * Set the expiration timeout
	 * 
	 * @param timeout milliseconds
	 * @return 
	 */
	public ExpiringQueueAbstract setTimeout(int timeout) {
		this.timeout = timeout;
		return this;
	}

	/**
	 * Add object to Hash Map
	 */
	public Object put(Object key, Object value) {
		Long timesOut = System.currentTimeMillis() + timeout;
		if (false == addedTime.containsKey(timesOut)) addedTime.put(timesOut, new ArrayList<Object>());
		addedTime.get(timesOut).add(key);
		return super.put(key, value);
	}
	
	public void expireEntries() {
		Iterator it = addedTime.entrySet().iterator();
		Long now = System.currentTimeMillis();
		while (it.hasNext()) {
			Map.Entry pairs = (Map.Entry) it.next();
			if (now >= (Long) pairs.getKey()) {
				expireValues((ArrayList<Object>) pairs.getValue());
				it.remove();
			}
		}
	}
	
	protected void expireValues(ArrayList<Object> values) {
		for (Object value : values) expireValue(value);
	}
	
	abstract protected void expireValue(Object value);
	
	public void stop() {
		if (null == timer) return;

	}
	
	public void start() {
		int period = timeout / 10;
		if (period < 1) period = 1;
		timer = new Timer();
		timer.scheduleAtFixedRate(new QueueTimer(this), period, period);
	}
}

class QueueTimer extends TimerTask {

    private ExpiringQueueAbstract queue;

    public QueueTimer(ExpiringQueueAbstract queue) {
        super();
        this.queue = queue;
    }

    @Override
    public void run() {
        queue.expireEntries(); 
    }
}