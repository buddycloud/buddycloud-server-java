package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.forms.DataForm;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;

public class SearchSetTest extends IQTestHandler {

	private IQ request;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private ChannelManager channelManager;

	private SearchSet search;
	private JID sender;
	private JID receiver;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		channelManager = Mockito.mock(ChannelManager.class);

		search = new SearchSet(queue, channelManager);

		sender = new JID("channels.shakespeare.lit");
		receiver = new JID("romeo@shakespeare.lit/home");

		request = new IQ();
		request.setFrom(receiver);
		request.setType(IQ.Type.set);
		request.setTo(sender);
		Element query = request.getElement().addElement("query");
		query.addNamespace("", Search.NAMESPACE_URI);

		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(true);
	}

	@Test
	public void testOnlyAcceptsPacketsFromLocalUsers() throws Exception {

		Mockito.when(channelManager.isLocalJID(Mockito.any(JID.class)))
				.thenReturn(false);

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.cancel, error.getType());
		Assert.assertEquals(PacketError.Condition.not_allowed,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfDataFormAbsent() throws Exception {

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfNamespaceIncorrect() throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", "some:other:namespace");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfTypeIncorrect() throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addAttribute("type", "wrongtype");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfFieldFormTypeIsIncorrect() throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addAttribute("type", "submit");

		Element field = x.addElement("field");
		field.addAttribute("var", "NOT_FORM_TYPE");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfFieldFormTypeValueIsIncorrect()
			throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addAttribute("type", "submit");

		Element field = x.addElement("field");
		field.addAttribute("var", "FORM_TYPE");
		field.addElement("value").addText("not:search:type");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfTooFewFields() throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addAttribute("type", "submit");

		Element field = x.addElement("field");
		field.addAttribute("var", "FORM_TYPE");
		field.addElement("value").addText(DataForm.NAMESPACE);

		Element singleField = x.addElement("field");
		singleField.addAttribute("var", "page");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfContentValueIncorrect() throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addAttribute("type", "submit");

		Element field = x.addElement("field");
		field.addAttribute("var", "FORM_TYPE");
		field.addElement("value").addText(Search.NAMESPACE_URI);

		Element singleField = x.addElement("field");
		singleField.addAttribute("var", "content");
		singleField.addElement("value");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfAuthorValueIncorrect() throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addAttribute("type", "submit");

		Element field = x.addElement("field");
		field.addAttribute("var", "FORM_TYPE");
		field.addElement("value").addText(Search.NAMESPACE_URI);

		Element singleField = x.addElement("field");
		singleField.addAttribute("var", "author");
		singleField.addElement("value");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfPageValueIncorrect() throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addAttribute("type", "submit");

		Element field = x.addElement("field");
		field.addAttribute("var", "FORM_TYPE");
		field.addElement("value").addText(Search.NAMESPACE_URI);

		Element singleField = x.addElement("field");
		singleField.addAttribute("var", "page");
		singleField.addElement("value").setText("sausages");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorIfRppValueIncorrect() throws Exception {
		Element query = request.getElement().element("query");
		Element x = query.addElement("x");
		x.addAttribute("xmlns", DataForm.NAMESPACE);
		x.addAttribute("type", "submit");

		Element field = x.addElement("field");
		field.addAttribute("var", "FORM_TYPE");
		field.addElement("value").addText(Search.NAMESPACE_URI);

		Element singleField = x.addElement("field");
		singleField.addAttribute("var", "rpp");
		singleField.addElement("value").setText("bananas");

		search.process(request);
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.modify, error.getType());
		Assert.assertEquals(PacketError.Condition.bad_request,
				error.getCondition());
	}

	@Test
	public void testReturnsErrorOnChannelManagerException() throws Exception {
		Mockito.when(
				channelManager.performSearch(Mockito.any(JID.class),
						Mockito.any(List.class), Mockito.anyString(),
						Mockito.anyInt(), Mockito.anyInt())).thenThrow(
				new NodeStoreException());
		
		IQ stanza = readStanzaAsIq("/iq/search/set.stanza");
		search.process(stanza);
		
		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.wait, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}
}