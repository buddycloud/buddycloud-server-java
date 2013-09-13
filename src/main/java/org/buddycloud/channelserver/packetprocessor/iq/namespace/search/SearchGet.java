package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.concurrent.BlockingQueue;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetprocessor.PacketProcessor;
import org.dom4j.Element;
import org.xmpp.forms.DataForm;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Type;

public class SearchGet implements PacketProcessor<IQ> {

	public static final String INSTRUCTIONS = "Search for content/hashtags/mentions";

	public static final String TITLE = "Please populate one or more of the following fields";
	
	private ChannelManager channelManager;
	private BlockingQueue<Packet> outQueue;
	private IQ response;

	private Element x;

	public SearchGet(BlockingQueue<Packet> outQueue,
			ChannelManager channelManager) {
		this.channelManager = channelManager;
		this.outQueue = outQueue;
	}

	@Override
	public void process(IQ request) throws Exception {
		response = IQ.createResultIQ(request);

		if (false == channelManager.isLocalJID(request.getFrom())) {
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
