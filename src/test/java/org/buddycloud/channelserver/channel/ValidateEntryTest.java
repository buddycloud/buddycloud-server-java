package org.buddycloud.channelserver.channel;

import java.util.concurrent.LinkedBlockingQueue;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.buddycloud.channelserver.packetHandler.iq.TestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.RepliesGet;
import org.dom4j.Element;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * TODO Additional work required:
 * 
 * - Check for (and add) activity stream verb 
 * 
 * - Check for (and add) activity stream object type 
 * 
 * - Check for (and add) content type (accept 'text' and 'html', default 'text') 
 * 
 * - If in-reply-to set 'verb' to comment
 * 
 * - Test 'geoloc' if present
 * 
 * - Test 'media' if present
 * 
 * - Test 'meta' if present
 * 
 */
public class ValidateEntryTest extends TestHandler {

	ValidateEntry validateEntry;
	IQ publishRequest;
	Element publishEntry;
	IQ replyRequest;
	Element replyEntry;
	
	JID jid = new JID("juliet@shakespeare.lit/balcony");
	String node = "/users/romeo@shakespeare.lit/posts";
    String server = "channels.shakespeare.lit";

	@Before
	public void setUp() throws Exception {
		publishRequest = readStanzaAsIq("/iq/pubsub/publish/request.stanza");
		publishEntry = publishRequest.getChildElement().element("publish")
				.element("item").element("entry");
		replyRequest = readStanzaAsIq("/iq/pubsub/publish/reply.stanza");
		replyEntry = replyRequest.getChildElement().element("publish")
				.element("item").element("entry");
	}

	@Test
	public void notProvidingAnEntryReturnsError() throws Exception {

		validateEntry = new ValidateEntry(null);
		Assert.assertFalse(validateEntry.isValid());
	}

	@Test
	public void missingIdAttributeGetsAdded() throws Exception {

		Assert.assertEquals("96da02ee1baef61e767742844207bec4",
				publishEntry.elementText("id"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("id").detach();
		validateEntry = new ValidateEntry(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.createBcCompatible(jid.toBareJID(), server, node);
		Assert.assertTrue(entry.elementText("id").contains("tag:" + server + "," + node + ","));
	}

	@Test
	public void emptyIdElementHasValueAdded() throws Exception {

		Assert.assertEquals("96da02ee1baef61e767742844207bec4",
				publishEntry.elementText("id"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("id").detach();
		entry.addElement("id");

		validateEntry = new ValidateEntry(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.createBcCompatible(jid.toBareJID(), server, node);
		Assert.assertTrue(entry.elementText("id").contains("tag:" + server + "," + node + ","));
	}

	@Test
	public void missingTitleElementIsAdded() throws Exception {

		Assert.assertEquals("Post title", publishEntry.elementText("title"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("title").detach();
		validateEntry = new ValidateEntry(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.createBcCompatible(jid.toBareJID(), server, node);
		Assert.assertEquals("Post", entry.elementText("title"));
	}

	@Test
	public void missingContentElementReturnsInvalid() throws Exception {

		Assert.assertNotNull(publishEntry.element("content"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("content").detach();
		validateEntry = new ValidateEntry(entry);
		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.MISSING_CONTENT_ELEMENT,
				validateEntry.getErrorMessage());
	}

	@Test
	public void missingUpdatedElementHasValueAdded() throws Exception {

		Assert.assertEquals("2014-01-01T00:00:00.000Z",
				publishEntry.elementText("updated"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("updated").detach();
		validateEntry = new ValidateEntry(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.createBcCompatible(jid.toBareJID(), server, node);
		Assert.assertTrue(entry
				.elementText("updated")
				.matches(
						"[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}Z"));
	}

	@Test
	public void globalInReplyToIdIsMadeALocalId() throws Exception {

		Assert.assertEquals(
				"null@channels.shakespeare.lit,/users/romeo@shakespeare.lit/posts,fc362eb42085f017ed9ccd9c4004b095",
				replyEntry.element("in-reply-to").attributeValue("ref"));

		Element entry = (Element) this.replyEntry.clone();
		validateEntry = new ValidateEntry(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.createBcCompatible(jid.toBareJID(), server, node);
		Assert.assertEquals("fc362eb42085f017ed9ccd9c4004b095",
				entry.element("in-reply-to").attributeValue("ref"));
	}
}
