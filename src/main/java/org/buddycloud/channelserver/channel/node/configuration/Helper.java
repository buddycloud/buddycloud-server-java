package org.buddycloud.channelserver.channel.node.configuration;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.channel.node.configuration.field.AccessModel;
import org.buddycloud.channelserver.channel.node.configuration.field.Affiliation;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelDescription;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelTitle;
import org.buddycloud.channelserver.channel.node.configuration.field.ChannelType;
import org.buddycloud.channelserver.channel.node.configuration.field.ConfigurationFieldException;
import org.buddycloud.channelserver.channel.node.configuration.field.ContentType;
import org.buddycloud.channelserver.channel.node.configuration.field.CreationDate;
import org.buddycloud.channelserver.channel.node.configuration.field.Creator;
import org.buddycloud.channelserver.channel.node.configuration.field.Factory;
import org.buddycloud.channelserver.channel.node.configuration.field.Field;
import org.buddycloud.channelserver.channel.node.configuration.field.LastUpdatedDate;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Message;

public class Helper {
	private HashMap<String, Field> elements;
	private Factory fieldFactory;
	private ChannelManager channelManager;

	public static final String FORM_TYPE = "http://jabber.org/protocol/pubsub#node_config";
	private static final String ELEMENT_NOT_FOUND = "Required XMPP element not found";

	private static final Logger LOGGER = Logger.getLogger(Helper.class);
	private static final String NODE_NOT_SET = "node-not-set";

	private ArrayList<String> requiredFields = new ArrayList<String>();
	private HashMap<String, String> existingConfiguration;

	private String node;

	public Helper(ChannelManager channelManager) {
		setupRequiredFields();
		this.channelManager = channelManager;
	}

	public void setNode(String node) {
		this.node = node;
	}

	private String getNode() {
		if ((null == node) || (0 == node.length())) {
			throw new NodeConfigurationException(NODE_NOT_SET);
		}
		return this.node;
	}

	private void setupRequiredFields() {
		requiredFields.add(ChannelTitle.FIELD_NAME);
		requiredFields.add(ChannelDescription.FIELD_NAME);
		requiredFields.add(AccessModel.FIELD_NAME);
		requiredFields.add(Creator.FIELD_NAME);
		requiredFields.add(Affiliation.FIELD_NAME);
		requiredFields.add(ChannelType.FIELD_NAME);
		requiredFields.add(ContentType.FIELD_NAME);
		requiredFields.add(CreationDate.FIELD_NAME);
		requiredFields.add(LastUpdatedDate.FIELD_NAME);
	}

	public void parse(IQ request) throws NodeConfigurationException {
		try {
			parseConfiguration(getConfigurationValues(request));
		} catch (NullPointerException e) {
			LOGGER.debug(e);
			throw new NodeConfigurationException(ELEMENT_NOT_FOUND);
		} catch (ConfigurationFieldException e) {
			LOGGER.debug(e);
			throw new NodeConfigurationException();
		}
	}

	public void parseEventUpdate(Message request)
			throws NodeConfigurationException {
		try {
			parseConfiguration(getConfigurationValuesFromEvent(request));
		} catch (NullPointerException e) {
			LOGGER.debug(e);
			throw new NodeConfigurationException(ELEMENT_NOT_FOUND);
		} catch (ConfigurationFieldException e) {
			e.printStackTrace();
			LOGGER.debug(e);
			throw new NodeConfigurationException();
		}
	}

	private List<FormField> getConfigurationValuesFromEvent(Message request) {
		Element element = request.getElement().element("event")
				.element("configuration").element("x");
		DataForm dataForm = new DataForm(element);
		List<FormField> fields = dataForm.getFields();
		return fields;
	}

	public void parseDiscoInfo(IQ request) throws NodeConfigurationException {
		try {
			parseConfiguration(getConfigurationValuesFromDisco(request));
		} catch (NullPointerException e) {
			LOGGER.debug(e);
			throw new NodeConfigurationException(ELEMENT_NOT_FOUND);
		} catch (ConfigurationFieldException e) {
			LOGGER.debug(e);
			throw new NodeConfigurationException();
		}
	}

	private List<FormField> getConfigurationValuesFromDisco(IQ request) {
		Element element = request.getElement().element("query").element("x");
		DataForm dataForm = new DataForm(element);
		List<FormField> fields = dataForm.getFields();
		return fields;
	}

	private List<FormField> getConfigurationValues(IQ request) {
		Element element = request.getElement().element("pubsub")
				.element("configure").element("x");
		DataForm dataForm = new DataForm(element);
		List<FormField> fields = dataForm.getFields();
		return fields;
	}

	private void parseConfiguration(List<FormField> configurationValues) {
		elements = new HashMap<String, Field>();
		if (configurationValues.isEmpty()) {
			return;
		}
		for (FormField configurationValue : configurationValues) {
			Field field = getFieldFactory().create(
					configurationValue.getVariable(),
					configurationValue.getFirstValue());
			elements.put(field.getName(), field);
		}
	}

	private Factory getFieldFactory() {
		if (fieldFactory == null) {
			fieldFactory = new Factory();
		}
		return fieldFactory;
	}

	public void setFieldFactory(Factory factory) {
		fieldFactory = factory;
	}

	public boolean isValid() {
		for (Entry<String, Field> element : elements.entrySet()) {
			if (!element.getValue().isValid()) {
				LOGGER.debug("Configuration field "
						+ element.getValue().getName()
						+ " is not valid with value "
						+ element.getValue().getValue());
				return false;
			}
		}
		
		return true;
	}

	public HashMap<String, String> getValues() throws NodeStoreException {
		existingConfiguration = (HashMap<String, String>) channelManager.getNodeConf(getNode());

		HashMap<String, String> data = new HashMap<String, String>();
		for (Entry<String, Field> element : elements.entrySet()) {
			String value = element.getValue().getValue();
			String key = element.getValue().getName();
			LOGGER.trace("For '" + key + "' we are storing value '" + value
					+ "'");
			data.put(key, value);
		}
		cleanData(data);
		addRequiredFields(data);
		return data;
	}
	
	private void addRequiredFields(HashMap<String, String> data) {
		for (String field : requiredFields) {
			if (!data.containsKey(field)) {
				if (existingConfiguration.containsKey(field)) {
				    data.put(field, existingConfiguration.get(field));
				} else if (elements.containsKey(field)) {
					data.put(field, elements.get(field).getValue());
				}
			}
		}
	}

	private void cleanData(HashMap<String, String> data) {
		preventOverwrite(data, ChannelType.FIELD_NAME);
		preventOverwrite(data, CreationDate.FIELD_NAME);
		preventOverwrite(data, Creator.FIELD_NAME);
	}

	private void preventOverwrite(HashMap<String, String> data, String fieldName) {
		if (!data.containsKey(fieldName)
				|| !existingConfiguration.containsKey(fieldName)) {
			return;
		}
		data.remove(fieldName);
		data.put(fieldName, existingConfiguration.get(fieldName));
	}
}