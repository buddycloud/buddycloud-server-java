package org.buddycloud.channelserver.packetprocessor.iq.namespace.search;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.db.ClosableIteratorImpl;
import org.buddycloud.channelserver.db.CloseableIterator;
import org.buddycloud.channelserver.db.exception.NodeStoreException;
import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.JabberPubsub;
import org.buddycloud.channelserver.pubsub.accessmodel.AccessModels;
import org.buddycloud.channelserver.pubsub.affiliation.Affiliations;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.NodeSubscription;
import org.buddycloud.channelserver.pubsub.model.impl.NodeAffiliationImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeSubscriptionImpl;
import org.buddycloud.channelserver.pubsub.subscription.Subscriptions;
import org.buddycloud.channelserver.utils.node.NodeAclRefuseReason;
import org.buddycloud.channelserver.utils.node.NodeViewAcl;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;
import org.dom4j.tree.BaseElement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.forms.DataForm;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.xmpp.packet.PacketError;
import org.xmpp.resultsetmanagement.ResultSetImpl;

public class SearchGetTest extends IQTestHandler {

	private IQ request;
	private Element element;
	private BlockingQueue<Packet> queue = new LinkedBlockingQueue<Packet>();

	private ChannelManager channelManager;

	private SearchGet search;
	private JID sender;
	private JID receiver;

	@Before
	public void setUp() throws Exception {

		queue = new LinkedBlockingQueue<Packet>();
		channelManager = Mockito.mock(ChannelManager.class);

		search = new SearchGet(queue, channelManager);

		sender = new JID("channels.shakespeare.lit");
		receiver = new JID("romeo@shakespeare.lit/home");
		
		request = new IQ();
		request.setFrom(receiver);
		request.setType(IQ.Type.get);
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
	public void testReturnsQueryChildElement() throws Exception {
		
		search.process(request);
		
		Assert.assertEquals(1, queue.size());
		
		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());

		Assert.assertEquals(receiver, response.getTo());
		Assert.assertEquals(sender, response.getFrom());
		Assert.assertEquals(IQ.Type.result, response.getType());

		Element query = response.getElement().element("query");
		Assert.assertNotNull(query);

		Assert.assertEquals(Search.NAMESPACE_URI, query.attributeValue("xmlns"));
	}

	@Test
	public void testReturnsInstructionsElement() throws Exception {
		
		search.process(request);
		
		Assert.assertEquals(1, queue.size());
		
		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());

		Assert.assertEquals(receiver, response.getTo());
		Assert.assertEquals(sender, response.getFrom());
		Assert.assertEquals(IQ.Type.result, response.getType());

		String instructions = response.getElement()
				.element("query")
				.elementText("instructions");
		Assert.assertEquals(SearchGet.INSTRUCTIONS, instructions);
	}
	
	  
	@Test
	public void testReturnsDataFormElement() throws Exception {
		
		search.process(request);
		
		Assert.assertEquals(1, queue.size());
		
		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());

		Assert.assertEquals(receiver, response.getTo());
		Assert.assertEquals(sender, response.getFrom());
		Assert.assertEquals(IQ.Type.result, response.getType());

		Element x = response.getElement()
				.element("query")
				.element("x");
	    Assert.assertNotNull(x);
	    Assert.assertEquals(DataForm.NAMESPACE, x.attributeValue("xmlns"));
	}
 
	@Test
	public void testReturnsDataFormTitleElement() throws Exception {
		
		search.process(request);
		
		Assert.assertEquals(1, queue.size());
		
		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());

		Assert.assertEquals(receiver, response.getTo());
		Assert.assertEquals(sender, response.getFrom());
		Assert.assertEquals(IQ.Type.result, response.getType());

	    String title = response.getElement()
				.element("query")
				.element("x")
				.elementText("title");
	    Assert.assertNotNull(title);
	    Assert.assertEquals(SearchGet.TITLE, title);
	}

    @Test 
    public void testReturnsDataFormInstructionsElement() throws Exception {
		
		search.process(request);
		
		Assert.assertEquals(1, queue.size());
		
		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());

		Assert.assertEquals(receiver, response.getTo());
		Assert.assertEquals(sender, response.getFrom());
		Assert.assertEquals(IQ.Type.result, response.getType());

	    String instructions = response.getElement()
				.element("query")
				.element("x")
				.elementText("instructions");
	    Assert.assertNotNull(instructions);
	    Assert.assertEquals(SearchGet.INSTRUCTIONS, instructions);
	}

	@Test
	public void testReturnsDataFormTypeElement() throws Exception {
		
		search.process(request);
		
		Assert.assertEquals(1, queue.size());
		
		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());

		Assert.assertEquals(receiver, response.getTo());
		Assert.assertEquals(sender, response.getFrom());
		Assert.assertEquals(IQ.Type.result, response.getType());

	    Element formType = (Element) response.getElement()
				.element("query")
				.element("x")
				.elements("field").get(0);
	    Assert.assertEquals(Search.NAMESPACE_URI, formType.elementText("value"));
	    Assert.assertEquals("hidden", formType.attributeValue("type"));
	    Assert.assertEquals("FORM_TYPE", formType.attributeValue("var"));
	}
	
	@Test
	public void testReturnsDataFormContentElement() throws Exception {
		
		search.process(request);
		
		Assert.assertEquals(1, queue.size());
		
		IQ response = (IQ) queue.poll();
		Assert.assertNull(response.getError());

		Assert.assertEquals(receiver, response.getTo());
		Assert.assertEquals(sender, response.getFrom());
		Assert.assertEquals(IQ.Type.result, response.getType());

	    Element formType = (Element) response.getElement()
				.element("query")
				.element("x")
				.elements("field").get(1);
	    Assert.assertEquals("text-multi", formType.attributeValue("type"));
	    Assert.assertEquals("content", formType.attributeValue("var"));
	    Assert.assertEquals(SearchGet.CONTENT_FIELD_LABEL, formType.attributeValue("label"));
		
	}
	 /* 
	 * @Test public void testReturnsDataFormAuthorElement() throws Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormResultsPerPageElement() throws
	 * Exception {
	 * 
	 * 
	 * }
	 * 
	 * @Test public void testReturnsDataFormPageElement() throws Exception {
	 * 
	 * 
	 * }
	 */
}