package org.buddycloud.channelserver.channel;

import java.util.concurrent.LinkedBlockingQueue;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.Packet;
import org.buddycloud.channelserver.packetHandler.iq.TestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.RepliesGet;
import org.dom4j.Element;

import junit.framework.Assert;
import junit.framework.TestCase;

public class ValidateEntryTest extends TestHandler {

	ValidateEntry validateEntry;
	IQ request;
	Element entry;

	@Before
	public void setUp() throws Exception {
		request = readStanzaAsIq("/iq/pubsub/publish/request.stanza");
		entry = request.getChildElement().element("publish").element("item")
				.element("entry");
	}

	@Test
	public void notProvidingAnEntryReturnsError() throws Exception {

		validateEntry = new ValidateEntry(null);
		Assert.assertFalse(validateEntry.isValid());
	}

	@Test
	public void missingIdAttributeGetsAdded() throws Exception {

		Assert.assertEquals("96da02ee1baef61e767742844207bec4", entry.elementText("id"));
		
		Element entry = (Element) this.entry.clone();
		entry.element("id").detach();
		validateEntry = new ValidateEntry(entry);
		Assert.assertTrue(validateEntry.isValid());
		Assert.assertEquals("1", entry.elementText("id"));
	}
	
	@Test
	public void emptyIdElementHasValueAdded() throws Exception {

		Assert.assertEquals("96da02ee1baef61e767742844207bec4", entry.elementText("id"));
		
		Element entry = (Element) this.entry.clone();
		entry.element("id").detach();
		entry.addElement("id");
		
		validateEntry = new ValidateEntry(entry);
		Assert.assertTrue(validateEntry.isValid());
		Assert.assertEquals("1", entry.elementText("id"));
	}
	
	@Test
	public void missingTitleElementIsAdded() throws Exception {

		Assert.assertEquals("Post title", entry.elementText("title"));
		
		Element entry = (Element) this.entry.clone();
		entry.element("title").detach();
		validateEntry = new ValidateEntry(entry);
		Assert.assertTrue(validateEntry.isValid());
		Assert.assertEquals("Post", entry.elementText("title"));
	}
}
