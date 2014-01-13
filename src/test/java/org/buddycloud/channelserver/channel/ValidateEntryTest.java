package org.buddycloud.channelserver.channel;

import java.util.Date;
import java.util.concurrent.LinkedBlockingQueue;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;
import org.buddycloud.channelserver.packetHandler.iq.TestHandler;
import org.buddycloud.channelserver.packetprocessor.iq.namespace.pubsub.get.RepliesGet;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.GlobalItemIDImpl;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.dom4j.Element;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * TODO Additional work required:
 * 
 * - Check for (and add) activity stream verb
 * 
 * - Test 'media' if present
 * 
 * - Test 'meta' if present
 * 
 */
public class ValidateEntryTest extends TestHandler {

	private ValidateEntry validateEntry;

	private IQ publishRequest;
	private Element publishEntry;
	private IQ replyRequest;
	private Element replyEntry;
	private IQ ratingRequest;
	private Element ratingEntry;

	private ChannelManager channelManager;

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
		ratingRequest = readStanzaAsIq("/iq/pubsub/publish/rating.stanza");
		ratingEntry = ratingRequest.getChildElement().element("publish")
				.element("item").element("entry");

		channelManager = Mockito.mock(ChannelManager.class);

		NodeItem item = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node),
						Mockito.anyString())).thenReturn(item);

	}

	private ValidateEntry getEntryObject(Element entry) {
		ValidateEntry validate = new ValidateEntry(entry);
		validate.setNode(node);
		validate.setTo(server);
		validate.setUser(jid);
		validate.setChannelManager(channelManager);
		return validate;
	}

	@Test
	public void notProvidingAnEntryReturnsError() throws Exception {

		validateEntry = new ValidateEntry(null);
		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.MISSING_ENTRY_ELEMENT,
				validateEntry.getErrorMessage());
	}

	@Test
	public void missingIdAttributeGetsAdded() throws Exception {

		Assert.assertEquals("96da02ee1baef61e767742844207bec4",
				publishEntry.elementText("id"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("id").detach();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertTrue(entry.elementText("id").contains(
				"tag:" + server + "," + node + ","));
	}

	@Test
	public void emptyIdElementHasValueAdded() throws Exception {

		Assert.assertEquals("96da02ee1baef61e767742844207bec4",
				publishEntry.elementText("id"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("id").detach();
		entry.addElement("id");

		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertTrue(entry.elementText("id").contains(
				"tag:" + server + "," + node + ","));
	}

	@Test
	public void idElementIsIgnored() throws Exception {
		String id = "96da02ee1baef61e767742844207bec4";
		Assert.assertEquals(id, publishEntry.elementText("id"));

		Element entry = (Element) this.publishEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertFalse(entry.elementText("id").contains(id));

	}

	@Test
	public void missingTitleElementIsAdded() throws Exception {

		Assert.assertEquals("Post title", publishEntry.elementText("title"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("title").detach();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertEquals("Post", entry.elementText("title"));
	}

	@Test
	public void missingContentElementReturnsInvalid() throws Exception {

		Assert.assertNotNull(publishEntry.element("content"));

		Element entry = (Element) this.publishEntry.clone();
		entry.element("content").detach();
		validateEntry = getEntryObject(entry);
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
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertTrue(entry
				.elementText("updated")
				.matches(
						"[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}Z"));
	}

	@Test
	public void updateDateIsIgnored() throws Exception {
		String dateString = "2014-01-01T00:00:00.000Z";
		Assert.assertEquals(dateString, publishEntry.elementText("updated"));

		Element entry = (Element) this.publishEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertFalse(entry.elementText("updated").equals(dateString));
	}

	@Test
	public void authorEntryIsAdded() throws Exception {
		Element entry = (Element) this.publishEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();

		Element author = entry.element("author");
		Assert.assertNotNull(author);
		Assert.assertEquals(ValidateEntry.AUTHOR_URI_PREFIX + jid.toBareJID(),
				author.elementText("uri"));
		Assert.assertEquals(jid.toBareJID(), author.elementText("name"));
		Assert.assertEquals(ValidateEntry.AUTHOR_TYPE,
				author.elementText("object-type"));
	}

	@Test
	public void geolocationEntryIsAdded() throws Exception {
		Element entry = (Element) this.publishEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();

		Assert.assertNotNull(entry.element("geoloc"));

		entry = (Element) this.replyEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();

		Assert.assertNull(entry.element("geoloc"));
	}

	@Test
	public void globalInReplyToIdIsMadeALocalId() throws Exception {

		Assert.assertEquals(
				"tag:channels.shakespeare.lit,/users/romeo@shakespeare.lit/posts,fc362eb42085f017ed9ccd9c4004b095",
				replyEntry.element("in-reply-to").attributeValue("ref"));

		Element entry = (Element) this.replyEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertEquals("fc362eb42085f017ed9ccd9c4004b095",
				entry.element("in-reply-to").attributeValue("ref"));
	}

	@Test
	public void postGetsNoteTypeReplyGetsCommentType() throws Exception {

		Element entry = (Element) this.publishEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();

		Assert.assertEquals(ValidateEntry.POST_TYPE_NOTE,
				entry.element("object").elementText("object-type"));

		entry = (Element) this.replyEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();

		Assert.assertEquals(ValidateEntry.POST_TYPE_COMMENT,
				entry.element("object").elementText("object-type"));
	}

	@Test
	public void badContentTypeReturnsError() throws Exception {
		Element entry = (Element) this.publishEntry.clone();
		entry.element("content").attribute("type").setText("emojis");
		validateEntry = getEntryObject(entry);
		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.UNSUPPORTED_CONTENT_TYPE,
				validateEntry.getErrorMessage());
	}

	@Test
	public void noContentTypeGetsDefaultedToText() throws Exception {
		Element entry = (Element) this.publishEntry.clone();
		entry.element("content").attribute("type").detach();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertEquals(ValidateEntry.CONTENT_TEXT, entry
				.element("content").attributeValue("type"));

	}

	@Test
	public void contentTypeGetsAddedAsRequired() throws Exception {
		Element entry = (Element) this.publishEntry.clone();
		entry.element("content").attribute("type")
				.setText(ValidateEntry.CONTENT_XHTML);
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();
		Assert.assertEquals(ValidateEntry.CONTENT_XHTML,
				entry.element("content").attributeValue("type"));
	}

	@Test
	public void activityVerbIsAdded() throws Exception {
		Element entry = (Element) this.publishEntry.clone();
		validateEntry = getEntryObject(entry);
		Assert.assertTrue(validateEntry.isValid());
		entry = validateEntry.getPayload();

		Assert.assertEquals(ValidateEntry.ACTIVITY_VERB_POST,
				entry.elementText("verb"));
	}

	@Test
	public void replyParentItemNotFoundResultsInError() throws Exception {

		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node),
						Mockito.anyString())).thenReturn(null);

		Element entry = (Element) this.replyEntry.clone();
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.PARENT_ITEM_NOT_FOUND,
				validateEntry.getErrorMessage());
	}

	@Test
	public void canNotReplyToAReply() throws Exception {

		NodeItem item = new NodeItemImpl(node, "2", new Date(), "<entry/>", "1");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node),
						Mockito.anyString())).thenReturn(item);

		Element entry = (Element) this.replyEntry.clone();
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.MAX_THREAD_DEPTH_EXCEEDED,
				validateEntry.getErrorMessage());
	}

	@Test
	public void targetElementWithoutInReplyToReturnsError() throws Exception {

		NodeItem item = new NodeItemImpl(node, "2", new Date(), "<entry/>", "1");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
				.thenReturn(item);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(null);

		Element entry = (Element) this.ratingEntry.clone();

		entry.element("in-reply-to").detach();
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.IN_REPLY_TO_MISSING,
				validateEntry.getErrorMessage());
	}

	@Test
	public void missingTargetIdElementReturnsError() throws Exception {

		NodeItem item = new NodeItemImpl(node, "2", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
				.thenReturn(item);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(null);

		Element entry = (Element) this.ratingEntry.clone();

		entry.element("target").element("id").detach();
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.MISSING_TARGET_ID,
				validateEntry.getErrorMessage());
	}

	@Test
	public void emptyTargetIdElementReturnsError() throws Exception {

		NodeItem item = new NodeItemImpl(node, "2", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
				.thenReturn(item);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(null);

		Element entry = (Element) this.ratingEntry.clone();

		entry.element("target").element("id").detach();
		entry.element("target").addElement("id");
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.MISSING_TARGET_ID,
				validateEntry.getErrorMessage());
	}

	@Test
	public void ifTargetedPostDoesntExistErrorIsReturned() throws Exception {

		NodeItem item = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
				.thenReturn(item);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(null);

		Element entry = (Element) this.ratingEntry.clone();
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.TARGETED_ITEM_NOT_FOUND,
				validateEntry.getErrorMessage());
	}

	@Test
	public void ifTargetedPostIsntInSameThreadErrorIsReturnedParentCheck()
			throws Exception {
		NodeItem item = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
				.thenReturn(item);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(item);

		Element entry = (Element) this.ratingEntry.clone();
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.TARGET_MUST_BE_IN_SAME_THREAD,
				validateEntry.getErrorMessage());
	}

	@Test
	public void ifTargetedPostIsntInSameThreadErrorIsReturnedThreadCheck()
			throws Exception {

		NodeItem item1 = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		NodeItem item2 = new NodeItemImpl(node, "B", new Date(), "<entry/>",
				"A");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
				.thenReturn(item1);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(item2);

		Element entry = (Element) this.ratingEntry.clone();
		entry.element("target").element("id").setText("B");
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.TARGET_MUST_BE_IN_SAME_THREAD,
				validateEntry.getErrorMessage());
	}

	@Test
	public void ifTargetedIdIsSameAsReplyToIdOnlyOneDatabaseLookupPerformed()
			throws Exception {
		NodeItem item = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
				.thenReturn(item);

		Element entry = (Element) this.ratingEntry.clone();
		entry.element("target").element("id").setText("1");

		validateEntry = getEntryObject(entry);

		validateEntry.isValid();

		Mockito.verify(channelManager, Mockito.times(1)).getNodeItem(
				Mockito.eq(node), Mockito.eq("1"));
	}

	@Test
	public void targetElementGetsAddedAsExpected() throws Exception {

		Element entry = (Element) this.ratingEntry.clone();
		entry.element("target").element("id").setText("1");

		validateEntry = getEntryObject(entry);

		Assert.assertTrue(validateEntry.isValid());

		Element payload = validateEntry.getPayload();

		String expectedId = "tag:channels.shakespeare.lit,/users/romeo@shakespeare.lit/posts,1";
		Assert.assertEquals(expectedId,
				payload.element("target").elementText("id"));
		Assert.assertEquals("post",
				payload.element("target").elementText("object-type"));
	}

	@Test
	public void missingInReplyToErrorsIfRatingElementPresent() throws Exception {

		Element entry = (Element) this.ratingEntry.clone();
		entry.element("in-reply-to").detach();
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.IN_REPLY_TO_MISSING,
				validateEntry.getErrorMessage());
	}

	@Test
	public void missingTargetErrorsIfRatingElementPresent() throws Exception {

		Element entry = (Element) this.ratingEntry.clone();
		entry.element("target").element("id").setText("1");
		entry.element("target").detach();
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.TARGET_ELEMENT_MISSING,
				validateEntry.getErrorMessage());
	}

	@Test
	public void invalidRatingValueReturnsError() throws Exception {
		
		Element entry = (Element) this.ratingEntry.clone();
		entry.element("target").element("id").setText("1");
		entry.element("rating").setText("awesome");
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.INVALID_RATING_VALUE,
				validateEntry.getErrorMessage());
	}

	@Test
	public void outOfRangeRatingReturnsError() throws Exception {
		
		Element entry = (Element) this.ratingEntry.clone();
		entry.element("rating").setText("6.0");
		entry.element("target").element("id").setText("1");
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.RATING_OUT_OF_RANGE,
				validateEntry.getErrorMessage());
	}
	
	@Test
	public void nonWholeNumberRatingReturnsError() throws Exception {
		Element entry = (Element) this.ratingEntry.clone();
		entry.element("rating").setText("4.1");
		entry.element("target").element("id").setText("1");
		validateEntry = getEntryObject(entry);

		Assert.assertFalse(validateEntry.isValid());
		Assert.assertEquals(ValidateEntry.INVALID_RATING_VALUE,
				validateEntry.getErrorMessage());
	}
	
    @Test
    public void ratingElementGetsAddedToPayloadAsExpected() throws Exception {
    	
    	String rating = "4";
    	
		Element entry = (Element) this.ratingEntry.clone();
		entry.element("rating").setText(rating);
		entry.element("target").element("id").setText("1");
		validateEntry = getEntryObject(entry);
		
		Assert.assertTrue(validateEntry.isValid());
		Element payload = validateEntry.getPayload();
		
		Assert.assertEquals(ValidateEntry.NS_REVIEW, payload.getNamespaceForPrefix("review").getText());
		Assert.assertEquals(rating + ".0", payload.element("rating").getTextTrim());
    }

}