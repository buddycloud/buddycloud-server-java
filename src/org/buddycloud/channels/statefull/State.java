package org.buddycloud.channels.statefull;

import java.util.Map;

public class State {

	public static final String KEY_STATE      = "state";
	public static final String KEY_COMPONENTS = "components";
	public static final String KEY_NODE       = "node";
	public static final String KEY_JID        = "jid";
	public static final String KEY_ID         = "id";
	
	public static final String STATE_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT = "subscribe-disco-items";
	public static final String STATE_DISCO_INFO_TO_COMPONENTS                 = "subscribe-disco-info";
	public static final String STATE_SUBSCRIBE                                = "subscribe";
	
	public static final String STATE_DISCOINFO_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT = "disco#info-disco-items";
	public static final String STATE_DISCOINFO_DISCO_INFO_TO_COMPONENTS                 = "disco#info-disco-info";
	public static final String STATE_DISCOINFO                                			= "disco#info";
	
	public static final String STATE_PUBLISH = "publish";
	
	//public static final String STATE_DISCO_ITEMS_TO_FIND_BC_CHANNEL_COMPONENT = "bc-disco-items";
	
	public static final String STATE_NOT_KNOWN = "not-known";
	
	private Map<String, String> store;
	
	public State(Map<String, String> store) {
		this.store = store;
	}
	
	public String getState() {
		return this.store.containsKey(KEY_STATE) ? this.store.get(KEY_STATE) : STATE_NOT_KNOWN;
	}
	
	public Map<String, String> getStore() {
		return this.store;
	}
}
