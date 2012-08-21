package org.buddycloud.channelserver.channel.node.configuration;

import java.util.HashMap;
import java.util.Map;

import org.buddycloud.channelserver.channel.node.configuration.field.Field;
import org.xmpp.packet.IQ;

public class Helper
{
	private HashMap<String, Field> elements = new HashMap<String, Field>();
	
	public static final String FORM_TYPE = "http://jabber.org/protocol/pubsub#node_config";
	
    public HashMap<String, String> parse(IQ dataForm)
    {
		return null;
    }

	public boolean isValid() 
	{
		for (Map.Entry<String, Field> element : elements.entrySet()) {
			if (false == element.getValue().isValid()) {
				return false;
			}
		}
		return true;
	}
	
	public HashMap<String, String> getValues()
	{
		HashMap<String, String> data = new HashMap<String, String>();
		for (Map.Entry<String, Field> element : elements.entrySet()) {
			String value = element.getValue().getValue();
			String key   = element.getValue().getName();
			data.put(key, value);
		}
		return data;
	}
}
