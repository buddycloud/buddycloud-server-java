package org.buddycloud.channelserver.channel.node.configuration;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.channel.node.configuration.field.ConfigurationFieldException;
import org.buddycloud.channelserver.channel.node.configuration.field.Factory;
import org.buddycloud.channelserver.channel.node.configuration.field.Field;
import org.buddycloud.channelserver.utils.xmlReader.XmlReader;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.Node;
import org.xmpp.packet.IQ;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.PacketExtension;

public class Helper
{
	protected HashMap<String, Field<?>> elements;
	
	HashMap<String, Field<?>> config;
	protected XmlReader    xmlReader;
	protected Factory      fieldFactory;

	public static final    String FORM_TYPE               = "http://jabber.org/protocol/pubsub#node_config";

	protected static final String NO_CONFIGURATION_VALUES = "No configuration values provided";
	protected static final String ELEMENT_NOT_FOUND       = "Required XMPP element not found";
	
    public void parse(IQ request) throws NodeConfigurationException
    {
        try {
            parseConfiguration(getConfigurationValues(request));
        } catch (NullPointerException e) {
        	throw new NodeConfigurationException(ELEMENT_NOT_FOUND);
        } catch (ConfigurationFieldException e) {
        	throw new NodeConfigurationException();
        }
    }

    private List<FormField> getConfigurationValues(IQ request)
    {
        Element element = request
        	.getElement()
        	.element("pubsub")
            .element("configure")
            .element("x");
        DataForm dataForm      = new DataForm(element);
        List<FormField> fields = dataForm.getFields();
        return fields;
    }

	private void parseConfiguration(List<FormField> configurationValues)
	{
        elements = new HashMap<String, Field<?>>();
		if (0 == configurationValues.size()) {
			return;
		}
		Field<?> field;
		for (FormField configurationValue : configurationValues) {
			field = getFieldFactory()
			    .create(configurationValue.getVariable(), configurationValue.getFirstValue());
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