package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
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
	private IQ setStanza;

	private String nodeItemNodeId1 = "/users/romeo@montague.lit/home";
	private String nodeItemNodeId2 = "/users/julet@capulet.lit/home";
	private String nodeItemId1 = "5w382609806986536982502859083409";
	private String nodeItemId2 = "fg455g542hg4hhtfgh4554hg5g5g54h4F";

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

		setStanza = readStanzaAsIq("/iq/search/set.stanza");
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

		search.process(setStanza);

		Packet response = queue.poll();
		PacketError error = response.getError();
		Assert.assertNotNull(error);
		Assert.assertEquals(PacketError.Type.wait, error.getType());
		Assert.assertEquals(PacketError.Condition.internal_server_error,
				error.getCondition());
	}

	@Test
	public void testNoResultsReturnsExpectedStanza() throws Exception {
		NodeItem[] items = new NodeItem[0];
		CloseableIterator<NodeItem> itemList = new ClosableIteratorImpl<NodeItem>(
				Arrays.asList(items).iterator());

		Mockito.doReturn(itemList)
				.when(channelManager)
				.performSearch(Mockito.any(JID.class), Mockito.any(List.class),
						Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt());

		search.process(setStanza);

		Packet response = queue.poll();
		Element query = response.getElement().element("query");
		Assert.assertNotNull(query);
		Assert.assertEquals(Search.NAMESPACE_URI, query.attributeValue("xmlns"));
		Assert.assertEquals(0, query.elements().size());
	}

	@Test
	public void testReturnsDataInExpectedFormat() throws Exception {
		NodeItemImpl item1 = new NodeItemImpl(nodeItemNodeId1, nodeItemId1,
				new Date(), "<entry/>");
		NodeItemImpl item2 = new NodeItemImpl(nodeItemNodeId2, nodeItemId2,
				new Date(), "<entry2/>");

		NodeItem[] itemArray = new NodeItem[2];
		itemArray[0] = item1;
		itemArray[1] = item2;

		CloseableIterator<NodeItem> itemList = new ClosableIteratorImpl<NodeItem>(
				Arrays.asList(itemArray).iterator());

		Mockito.doReturn(itemList)
				.when(channelManager)
				.performSearch(Mockito.any(JID.class), Mockito.any(List.class),
						Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt());

		search.process(setStanza);

		Packet response = queue.poll();
		Element query = response.getElement().element("query");
		Assert.assertNotNull(query);

		Element x = query.element("x");
		Assert.assertNotNull(x);

		Element field = x.element("field");
		Assert.assertNotNull(field);
		Assert.assertEquals("FORM_TYPE", field.attributeValue("var"));
		Assert.assertEquals(Search.NAMESPACE_URI, field.element("value")
				.getText());

		Element reported = x.element("reported");
		Assert.assertNotNull(reported);

		List<Element> fields = reported.elements("field");
		Assert.assertEquals(3, fields.size());

		Assert.assertEquals("node", fields.get(0).attributeValue("var"));
		Assert.assertEquals("Node", fields.get(0).attributeValue("label"));
		Assert.assertEquals("text-single", fields.get(0).attributeValue("type"));

		Assert.assertEquals("id", fields.get(1).attributeValue("var"));
		Assert.assertEquals("Item ID", fields.get(1).attributeValue("label"));
		Assert.assertEquals("text-single", fields.get(1).attributeValue("type"));

		Assert.assertEquals("entry", fields.get(2).attributeValue("var"));
		Assert.assertEquals("Item", fields.get(2).attributeValue("label"));
		Assert.assertEquals("http://www.w3.org/2005/Atom", fields.get(2)
				.attributeValue("type"));

		List<Element> items = x.elements("item");
		Assert.assertEquals(2, items.size());

		List<Element> itemFields = items.get(0).elements("field");
		Assert.assertEquals(3, itemFields.size());
		Assert.assertEquals("node", itemFields.get(0).attributeValue("var"));
		Assert.assertEquals(nodeItemNodeId1, itemFields.get(0).element("value")
				.getText());

		Assert.assertEquals("id", itemFields.get(1).attributeValue("var"));
		Assert.assertEquals(nodeItemId1, itemFields.get(1).element("value")
				.getText());

		Assert.assertEquals("entry", itemFields.get(2).attributeValue("var"));
		Assert.assertEquals(1,
				itemFields.get(2).element("value").elements("entry").size());

		itemFields = items.get(1).elements("field");
		Assert.assertEquals(3, itemFields.size());
		Assert.assertEquals("node", itemFields.get(0).attributeValue("var"));
		Assert.assertEquals(nodeItemNodeId2, itemFields.get(0).element("value")
				.getText());

		Assert.assertEquals("id", itemFields.get(1).attributeValue("var"));
		Assert.assertEquals(nodeItemId2, itemFields.get(1).element("value")
				.getText());

		Assert.assertEquals("entry", itemFields.get(2).attributeValue("var"));
		Assert.assertEquals(1,
				itemFields.get(2).element("value").elements("entry2").size());
	}

	@Test
	public void testBadlyFormedSourceDataIsIgnored() throws Exception {
		NodeItemImpl item1 = new NodeItemImpl(nodeItemNodeId1, nodeItemId1,
				new Date(), "<entry><open>");
		NodeItemImpl item2 = new NodeItemImpl(nodeItemNodeId2, nodeItemId2,
				new Date(), "<entry2/>");

		NodeItem[] itemArray = new NodeItem[2];
		itemArray[0] = item1;
		itemArray[1] = item2;

		CloseableIterator<NodeItem> itemList = new ClosableIteratorImpl<NodeItem>(
				Arrays.asList(itemArray).iterator());

		Mockito.doReturn(itemList)
				.when(channelManager)
				.performSearch(Mockito.any(JID.class), Mockito.any(List.class),
						Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt());

		search.process(setStanza);

		Packet response = queue.poll();
		Element query = response.getElement().element("query");

		Assert.assertNotNull(query);

		Element x = query.element("x");
		Assert.assertNotNull(x);

		List<Element> items = x.elements("item");
		Assert.assertEquals(1, items.size());

		List<Element> itemFields = items.get(0).elements("field");
		Assert.assertEquals(3, itemFields.size());
		Assert.assertEquals("node", itemFields.get(0).attributeValue("var"));
		Assert.assertEquals(nodeItemNodeId2, itemFields.get(0).element("value")
				.getText());

		Assert.assertEquals("id", itemFields.get(1).attributeValue("var"));
		Assert.assertEquals(nodeItemId2, itemFields.get(1).element("value")
				.getText());

		Assert.assertEquals("entry", itemFields.get(2).attributeValue("var"));
		Assert.assertEquals(1,
				itemFields.get(2).element("value").elements("entry2").size());
	}
}