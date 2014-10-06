package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.Configuration;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class SearchGet implements PacketProcessor<IQ> {

	public static final String INSTRUCTIONS = "Search for content/hashtags/mentions";

	public static final String TITLE = "Please populate one or more of the following fields";
	public static final String CONTENT_FIELD_LABEL = "Content search";
	public static final String AUTHOR_FIELD_LABEL = "Author";
	public static final String RPP_FIELD_LABEL = "Results per page";
	public static final String PAGE_FIELD_LABEL = "Page";
	
	private BlockingQueue<Packet> outQueue;
	private IQ response;

	private Element x;

	public SearchGet(BlockingQueue<Packet> outQueue) {
		this.outQueue = outQueue;
	}

	@Override
	public void process(IQ request) throws Exception {
		response = IQ.createResultIQ(request);

		if (false == Configuration.getInstance().isLocalJID(request.getFrom())) {
			sendErrorResponse(PacketError.Type.cancel,
					PacketError.Condition.not_allowed);
			return;
		}
		
		Element query = response.getElement().addElement("query");
		query.addAttribute("xmlns", Search.NAMESPACE_URI);
		query.addElement("instructions").addText(INSTRUCTIONS);
		x = query.addElement("x");
		addFields();
		outQueue.put(response);
	}

	private void addFields() {
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addElement("title").addText(TITLE);
		x.addElement("instructions").addText(INSTRUCTIONS);
		
		Element formType = x.addElement("field");
		formType.addAttribute("type", "hidden");
		formType.addAttribute("var", "FORM_TYPE");
		formType.addElement("value").addText(Search.NAMESPACE_URI);
		
		Element content = x.addElement("field");
		content.addAttribute("type", "text-multi");
		content.addAttribute("var", "content");
		content.addAttribute("label", CONTENT_FIELD_LABEL);
		
		Element author = x.addElement("field");
		author.addAttribute("type", "jid-single");
		author.addAttribute("var", "author");
		author.addAttribute("label", AUTHOR_FIELD_LABEL);
		
		Element rpp = x.addElement("field");
		rpp.addAttribute("type", "fixed");
		rpp.addAttribute("var", "rpp");
		rpp.addAttribute("label", RPP_FIELD_LABEL);
		
		Element page = x.addElement("field");
		page.addAttribute("type", "fixed");
		page.addAttribute("var", "page");
		page.addAttribute("label", PAGE_FIELD_LABEL);
	}

	private void sendErrorResponse(PacketError.Type type,
			PacketError.Condition condition) throws InterruptedException {
		response.setType(IQ.Type.error);
		PacketError error = new PacketError(PacketError.Condition.not_allowed,
				PacketError.Type.cancel);
		response.setError(error);
		outQueue.put(response);
	}

}
