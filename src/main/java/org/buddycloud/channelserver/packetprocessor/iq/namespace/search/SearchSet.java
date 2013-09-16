package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.forms.FormField;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class SearchSet implements PacketProcessor<IQ> {

	private ChannelManager channelManager;
	private BlockingQueue<Packet> outQueue;
	private IQ responseIq;
	private Element x;
	private Element query;
	private IQ requestIq;
	private ArrayList<String> content;
	private String author;
	private int page = 1;
	private int rpp = 25;
	private JID searcher;

	public SearchSet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.channelManager = channelManager;
		this.outQueue = outQueue;
	}

	@Override
	public void process(IQ request) throws Exception {
		searcher = request.getFrom();
		responseIq = IQ.createResultIQ(request);
		this.requestIq = request;

		if (false == isValidRequest()) {
			return;
		}

		if (false == processForm()) {
			return;
		}

		try {
			runSearch();
		} catch (NodeStoreException e) {
			sendErrorResponse(PacketError.Type.wait,
					PacketError.Condition.internal_server_error);
			return;
		}

		query = responseIq.getElement().addElement("query");
		query.addAttribute("xmlns", Search.NAMESPACE_URI);

		outQueue.put(responseIq);
	}

	private boolean isValidRequest() throws Exception {

		if (false == channelManager.isLocalJID(searcher)) {
			sendErrorResponse(PacketError.Type.cancel,
					PacketError.Condition.not_allowed);
			return false;
		}

		if (false == hasDataForm() || false == dataFormCorrect()) {
			return false;
		}

		return true;
	}

	private boolean hasDataForm() throws Exception {
		x = requestIq.getElement().element("query").element("x");

		if (null == x || !DataForm.NAMESPACE.equals(x.getNamespaceURI())
				|| !"submit".equals(x.attributeValue("type"))) {
			sendErrorResponse(PacketError.Type.modify,
					PacketError.Condition.bad_request);
			return false;
		}

		return true;
	}

	private boolean dataFormCorrect() throws Exception {
		if (!hasCorrectFormElement() || !hasEnoughFormFields()) {
			sendErrorResponse(PacketError.Type.modify,
					PacketError.Condition.bad_request);
			return false;
		}

		return true;
	}

	private boolean hasCorrectFormElement() throws Exception {

		List<Element> elements = x.elements("field");

		if (elements.size() > 0) {
			for (Element field : elements) {
				if (!"FORM_TYPE".equals(field.attributeValue("var"))) {
					continue;
				}

				String value = field.elementText("value");

				if (null == value || !Search.NAMESPACE_URI.equals(value)) {
					return false;
				}

				return true;
			}
		}

		return false;
	}

	private boolean hasEnoughFormFields() throws Exception {
		List<Element> elements = x.elements("field");
		if (elements.size() < 2) {
			return false;
		}

		boolean hasContentOrAuthor = false;

		String var;
		for (Element field : elements) {
			var = field.attributeValue("var");
			if ("content".equals(var) || "author".equals(var)) {
				hasContentOrAuthor = true;
			}
		}

		return hasContentOrAuthor;
	}

	private boolean processForm() throws Exception {
		try {
			extractFieldValues();
		} catch (NumberFormatException e) {
			return false;
		}

		if (false == checkFieldValues()) {
			return false;
		}

		return true;
	}

	private void runSearch() throws NodeStoreException {
		channelManager.performSearch(searcher, content, author, page, rpp);
	}

	private void extractFieldValues() {
		List<Element> elements = x.elements("field");
		String var;
		for (Element field : elements) {
			var = field.attributeValue("var");
			if ("content".equals(var)) {
				content = getValuesAsList(field);
			} else if ("author".equals(var)) {
				author = field.elementText("value");
			} else if ("page".equals(var)) {
				page = getValueAsNumber(field);
			} else if ("rpp".equals(var)) {
				rpp = getValueAsNumber(field);
			}
		}
	}

	private boolean checkFieldValues() throws Exception {
		if (((null != content && content.size() > 0) || (null != author && author
				.length() > 0)) && (page > 0 && rpp > 0)) {
			return true;
		}

		sendErrorResponse(PacketError.Type.modify,
				PacketError.Condition.bad_request);
		return false;

	}

	private ArrayList<String> getValuesAsList(Element field) {
		ArrayList<String> rtn = new ArrayList<String>();
		String valueText;
		for (Element value : (List<Element>) field.elements("value")) {
			valueText = value.getText();
			if (valueText.length() == 0) {
				continue;
			}
			rtn.add(valueText);
		}
		return rtn;
	}

	private Integer getValueAsNumber(Element field)
			throws NumberFormatException {
		String valueStr = field.elementText("value");
		return Integer.parseInt(valueStr);
	}

	private void sendErrorResponse(PacketError.Type type,
			PacketError.Condition condition) throws InterruptedException {
//		try {
//			throw new Exception();
//		} catch ( Exception e ) {
//			e.printStackTrace();
//		}
		responseIq.setType(IQ.Type.error);
		PacketError error = new PacketError(condition, type);
		responseIq.setError(error);

		outQueue.put(responseIq);
	}
}
