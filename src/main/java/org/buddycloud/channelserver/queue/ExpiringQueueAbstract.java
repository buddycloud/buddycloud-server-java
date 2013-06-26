package org.buddycloud.channelserver.queue;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.ChannelsEngine;

abstract public class ExpiringQueueAbstract extends ConcurrentHashMap<Object, Object> {

	private static Logger logger = Logger.getLogger(ExpiringQueueAbstract.class);
	private HashMap<Long, ArrayList> addedTime = new HashMap<Long, ArrayList>();
	
	private static final int DEFAULT_TIMEOUT = 180000;
	private int timeout = 0;

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
	 */
	public void setTimeout(int timeout) {
		this.timeout = timeout;
	}

	/**
	 * Add object to Hash Map
	 */
	public Object put(Object key, Object value) {
		Long now = new Date().getTime();
		if (false == addedTime.containsKey(now)) addedTime.put(now, new ArrayList<Object>());
		addedTime.get(now).add(key);
		return super.put(key, value);
	}
	
	public void expireEntries() {
		Iterator it = addedTime.entrySet().iterator();
		Long now = new Date().getTime();
		Long delay = (long) 0;
		while (it.hasNext()) {
			Map.Entry pairs = (Map.Entry) it.next();
			delay = now - (Long) pairs.getKey();
			if (delay >= timeout) {
				expireValues((ArrayList<Object>) pairs.getValue());
			}
			it.remove();
		}
	}
	
	protected void expireValues(ArrayList<Object> values) {
		for (Object value : values) expireValue(value);
	}
	
	abstract protected void expireValue(Object value);
}