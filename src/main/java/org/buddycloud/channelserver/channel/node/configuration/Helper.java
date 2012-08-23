package org.buddycloud.channelserver.channel.node.configuration;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.buddycloud.channelserver.channel.node.configuration.field.ConfigurationFieldException;
import org.buddycloud.channelserver.channel.node.configuration.field.Factory;
import org.buddycloud.channelserver.channel.node.configuration.field.Field;
import org.buddycloud.channelserver.utils.xmlReader.XmlReader;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.Node;
import org.xmpp.packet.IQ;

public class Helper
{
	protected HashMap<String, Field<?>> elements;
	
	HashMap<String, Field<?>> config;
	protected XmlReader    xmlReader;
	protected Factory      fieldFactory;

	public static final    String FORM_TYPE               = "http://jabber.org/protocol/pubsub#node_config";
	protected static final String NO_CONFIGURE_ELEMENT    = "No 'configure' element";
	protected static final String NO_CONFIGURATION_VALUES = "No configration values provided";
	
    public void parse(IQ request) throws NodeConfigurationException
    {
    	Element xml = convertIqToDomElementTree(request);
    	Node configureElement = xml.selectSingleNode("//iq/pubsub/configure");
        if (null == configureElement) {
       	    throw new NodeConfigurationException(NO_CONFIGURE_ELEMENT);
        }
        List<? extends Node> configurationValues = xml.selectNodes("//iq/pubsub/configure/x/field");
        try {
            parseConfiguration(configurationValues);
        } catch (ConfigurationFieldException e) {
        	throw new NodeConfigurationException();
        }
    }

	private void parseConfiguration(List configurationValues)
	{
		elements = new HashMap<String, Field<?>>();
		if (0 == configurationValues.size()) {
			return;
		}
		Field<?> field;
		for (Iterator<? extends Node> node = configurationValues.iterator(); node.hasNext();) {
			field = getFieldFactory().create(node.next());
			elements.put(
				field.getName(),
				field
		    );
		}
	}

	private Factory getFieldFactory()
	{
		if (null == fieldFactory) {
			fieldFactory = new Factory();
		}
		return fieldFactory;
	}
	
	public void setFieldFactory(Factory factory)
	{
		fieldFactory = factory;
	}

	private Element convertIqToDomElementTree(IQ request)
	{
		try {
			return getXmlParser().parse(request);
		} catch (DocumentException e) {
			throw new NodeConfigurationException();
		}
	}

	private XmlReader getXmlParser()
	{
		if (null == xmlReader) {
			xmlReader = new XmlReader();
		}
		return xmlReader;		
	}
	
	public void setXmlParser(XmlReader reader)
	{
		xmlReader = reader;
	}

	public boolean isValid() 
	{
		for (Entry<String, Field<?>> element : elements.entrySet()) {
			if (false == element.getValue().isValid()) {
				return false;
			}
		}
		return true;
	}
	
	public HashMap<String, String> getValues()
	{
		HashMap<String, String> data = new HashMap<String, String>();
		for (Entry<String, Field<?>> element : elements.entrySet()) {
			String value = element.getValue().getValue();
			String key   = element.getValue().getName();
			data.put(key, value);
		}
		return data;
	}
}