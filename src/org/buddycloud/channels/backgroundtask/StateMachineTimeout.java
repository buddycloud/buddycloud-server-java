package org.buddycloud.channels.backgroundtask;

import java.util.Map;
import java.util.Properties;

import org.buddycloud.channels.jedis.JedisKeys;
import org.buddycloud.channels.queue.ErrorQueue;
import org.buddycloud.channels.statefull.StateMachine;

import redis.clients.jedis.Jedis;

public class StateMachineTimeout {

	private ErrorQueue errorQueue = null;
	
	private Properties conf;
	
	public static int TIMEOUT_IN_SECONDS = 10;
	
	Thread worker = new Thread();
	
	private boolean keep_it_going_baby = true;
	
	public StateMachineTimeout(ErrorQueue errorQueue, Properties conf) {
		this.errorQueue = errorQueue;
		this.conf = conf;
		
		this.worker = new Thread(new Worker());
		this.worker.start();
	}
	
	public void stopBackgroundTask() {
		this.keep_it_going_baby = false;
	}
	
	private class Worker implements Runnable {

		private Jedis jedis;
		
		public Worker() {
			
			this.jedis = new Jedis(conf.getProperty("redis.host"), 
								   Integer.valueOf(conf.getProperty("redis.port")));
			this.jedis.configSet("timeout", "0");
			
		}
		
		@Override
		public void run() {
			
			Long start;
			
			StateMachine stateMachine = new StateMachine(this.jedis, null, errorQueue);
			Map<String, String> possibleTimeouts = null;
			while (keep_it_going_baby) {
				
				try {
				
					Thread.sleep(1000);
					
					possibleTimeouts = this.jedis.hgetAll(JedisKeys.STATE_TIMEOUTS);
					
					int compTime = ((int)(System.currentTimeMillis() / 1000) - TIMEOUT_IN_SECONDS);
					
					for (String id : possibleTimeouts.keySet()) {
						if(Integer.parseInt(possibleTimeouts.get(id)) >= compTime) {
							continue;
						}
						
						stateMachine.timeout(id);
					}
					
					possibleTimeouts = null;
					
				} catch (InterruptedException e) {
					
					//LogMe.warning("Error consuming OutQueue: '" + e.getMessage() + "'!");
					e.printStackTrace();
					
				} catch (Exception e) {
					
					//LogMe.warning("Error consuming OutQueue: '" + e.getMessage() + "'!");
					e.printStackTrace();
					
				}
			}
		}
	}
}
