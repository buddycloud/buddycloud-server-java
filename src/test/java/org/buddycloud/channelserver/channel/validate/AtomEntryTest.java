package org.buddycloud.channelserver.channel.validate;

import java.util.Date;

import junit.framework.Assert;

import org.buddycloud.channelserver.channel.ChannelManager;
import org.buddycloud.channelserver.packetHandler.iq.TestHandler;
import org.buddycloud.channelserver.pubsub.model.GlobalItemID;
import org.buddycloud.channelserver.pubsub.model.NodeItem;
import org.buddycloud.channelserver.pubsub.model.impl.NodeItemImpl;
import org.buddycloud.channelserver.utils.node.item.payload.ActivityStreams;
import org.buddycloud.channelserver.utils.node.item.payload.Buddycloud;
import org.dom4j.Element;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;

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
public class AtomEntryTest extends TestHandler {

	private AtomEntry validator;

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

    private Element emptyEntry;

	@Before
	public void setUp() throws Exception {
		publishRequest = readStanzaAsIq("/iq/pubsub/publish/request.stanza");
		publishEntry = publishRequest.getChildElement().element("publish")
                .element("item");
		replyRequest = readStanzaAsIq("/iq/pubsub/publish/reply.stanza");
		replyEntry = replyRequest.getChildElement().element("publish")
                .element("item");
		ratingRequest = readStanzaAsIq("/iq/pubsub/publish/rating.stanza");
		ratingEntry = ratingRequest.getChildElement().element("publish")
                .element("item");

		channelManager = Mockito.mock(ChannelManager.class);

		NodeItem item = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node),
						Mockito.anyString())).thenReturn(item);

	}

    private AtomEntry getEntryObject(Element item) {
        AtomEntry validate = new AtomEntry(item);
		validate.setNode(node);
		validate.setTo(server);
		validate.setUser(jid);
		validate.setChannelManager(channelManager);
		return validate;
	}

	@Test
	public void notProvidingAnEntryReturnsError() throws Exception {

		validator = new AtomEntry(null);
		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.MISSING_ENTRY_ELEMENT,
				validator.getErrorMessage());
	}

	@Test
	public void missingIdAttributeGetsAdded() throws Exception {

        Assert.assertEquals("96da02ee1baef61e767742844207bec4", publishEntry
                .element("entry").elementText("id"));

        Element item = (Element) this.publishEntry.clone();
        item.element("entry").element("id").detach();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
		Assert.assertTrue(entry.elementText("id").contains(
				"tag:" + server + "," + node + ","));
	}

	@Test
	public void emptyIdElementHasValueAdded() throws Exception {

        Assert.assertEquals("96da02ee1baef61e767742844207bec4", publishEntry
                .element("entry").elementText("id"));

        Element item = (Element) this.publishEntry.clone();
        item.element("entry").element("id").detach();
        item.element("entry").addElement("id");

        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
		Assert.assertTrue(entry.elementText("id").contains(
				"tag:" + server + "," + node + ","));
	}

	@Test
	public void idElementIsIgnored() throws Exception {
		String id = "96da02ee1baef61e767742844207bec4";
        Assert.assertEquals(id, publishEntry.element("entry").elementText("id"));

        Element item = (Element) this.publishEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
		Assert.assertFalse(entry.elementText("id").contains(id));

	}

	@Test
	public void missingTitleElementIsAdded() throws Exception {

        Assert.assertEquals("Post title", publishEntry.element("entry")
                .elementText("title"));

        Element item = (Element) this.publishEntry.clone();
        item.element("entry").element("title").detach();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
		Assert.assertEquals("Post", entry.elementText("title"));
	}

	@Test
	public void missingContentElementReturnsInvalid() throws Exception {

        Assert.assertNotNull(publishEntry.element("entry").element("content"));

        Element item = (Element) this.publishEntry.clone();
        item.element("entry").element("content").detach();
        validator = getEntryObject(item);
		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.MISSING_CONTENT_ELEMENT,
				validator.getErrorMessage());
	}

	@Test
	public void missingUpdatedElementHasValueAdded() throws Exception {

		Assert.assertEquals("2014-01-01T00:00:00.000Z",
                publishEntry.element("entry").elementText("updated"));

        Element item = (Element) this.publishEntry.clone();
        item.element("entry").element("updated").detach();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
		Assert.assertTrue(entry
				.elementText("updated")
				.matches(
						"[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}Z"));
	}

	@Test
	public void updateDateIsIgnored() throws Exception {
		String dateString = "2014-01-01T00:00:00.000Z";
        Assert.assertEquals(dateString, publishEntry.element("entry")
                .elementText("updated"));

        Element item = (Element) this.publishEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
		Assert.assertFalse(entry.elementText("updated").equals(dateString));
	}

	@Test
	public void authorEntryIsAdded() throws Exception {
        Element item = (Element) this.publishEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();

		Element author = entry.element("author");
		Assert.assertNotNull(author);
        Assert.assertEquals(AtomEntry.AUTHOR_URI_PREFIX + jid.toBareJID(),
				author.elementText("uri"));
		Assert.assertEquals(jid.toBareJID(), author.elementText("name"));
        Assert.assertEquals(AtomEntry.AUTHOR_TYPE,
				author.elementText("object-type"));
	}

	@Test
	public void geolocationEntryIsAdded() throws Exception {
        Element item = (Element) this.publishEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();

		Assert.assertNotNull(entry.element("geoloc"));

        item = (Element) this.replyEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
		entry = validator.getPayload();

		Assert.assertNull(entry.element("geoloc"));
	}

	@Test
	public void globalInReplyToIdIsMadeALocalId() throws Exception {

		Assert.assertEquals(
				"tag:channels.shakespeare.lit,/users/romeo@shakespeare.lit/posts,fc362eb42085f017ed9ccd9c4004b095",
                replyEntry.element("entry").element("in-reply-to")
                        .attributeValue("ref"));

        Element item = (Element) this.replyEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
		Assert.assertEquals("fc362eb42085f017ed9ccd9c4004b095",
				entry.element("in-reply-to").attributeValue("ref"));
	}

	@Test
	public void postGetsNoteTypeReplyGetsCommentType() throws Exception {

        Element item = (Element) this.publishEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();

        Assert.assertEquals(AtomEntry.POST_TYPE_NOTE, entry.element("object")
				.elementText("object-type"));

        item = (Element) this.replyEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
		entry = validator.getPayload();

        Assert.assertEquals(AtomEntry.POST_TYPE_COMMENT, entry
				.element("object").elementText("object-type"));
	}

	@Test
	public void badContentTypeReturnsError() throws Exception {
        Element item = (Element) this.publishEntry.clone();
        item.element("entry").element("content").attribute("type")
                .setText("emojis");
        validator = getEntryObject(item);
		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.UNSUPPORTED_CONTENT_TYPE,
				validator.getErrorMessage());
	}

	@Test
	public void noContentTypeGetsDefaultedToText() throws Exception {
        Element item = (Element) this.publishEntry.clone();
        item.element("entry").element("content").attribute("type").detach();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
        Assert.assertEquals(AtomEntry.CONTENT_TEXT, entry.element("content")
				.attributeValue("type"));

	}

	@Test
	public void contentTypeGetsAddedAsRequired() throws Exception {
        Element item = (Element) this.publishEntry.clone();
        item.element("entry").element("content").attribute("type")
                .setText(AtomEntry.CONTENT_XHTML);
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();
        Assert.assertEquals(AtomEntry.CONTENT_XHTML, entry.element("content")
				.attributeValue("type"));
	}

	@Test
	public void activityVerbIsAdded() throws Exception {
        Element item = (Element) this.publishEntry.clone();
        validator = getEntryObject(item);
		Assert.assertTrue(validator.isValid());
        Element entry = validator.getPayload();

        Assert.assertEquals(AtomEntry.ACTIVITY_VERB_POST,
				entry.elementText("verb"));
	}

	@Test
	public void replyParentItemNotFoundResultsInError() throws Exception {

		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node),
						Mockito.anyString())).thenReturn(null);

        Element item = (Element) this.replyEntry.clone();
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.PARENT_ITEM_NOT_FOUND,
				validator.getErrorMessage());
	}

	@Test
	public void canNotReplyToAReply() throws Exception {

        NodeItem nodeItem = new NodeItemImpl(node, "2", new Date(), "<entry/>", "1");

		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node),
                        Mockito.anyString())).thenReturn(nodeItem);

        Element item = (Element) this.replyEntry.clone();
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.MAX_THREAD_DEPTH_EXCEEDED,
				validator.getErrorMessage());
	}

	@Test
	public void targetElementWithoutInReplyToReturnsError() throws Exception {

        NodeItem nodeItem = new NodeItemImpl(node, "2", new Date(), "<entry/>", "1");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
                .thenReturn(nodeItem);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(null);

        Element item = (Element) this.ratingEntry.clone();

        item.element("entry").element("in-reply-to").detach();
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.IN_REPLY_TO_MISSING,
				validator.getErrorMessage());
	}

	@Test
	public void missingTargetIdElementReturnsError() throws Exception {

        NodeItem nodeItem = new NodeItemImpl(node, "2", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
                .thenReturn(nodeItem);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(null);

        Element item = (Element) this.ratingEntry.clone();

        item.element("entry").element("target").element("id").detach();
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.MISSING_TARGET_ID,
				validator.getErrorMessage());
	}

	@Test
	public void emptyTargetIdElementReturnsError() throws Exception {

        NodeItem nodeItem = new NodeItemImpl(node, "2", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
                .thenReturn(nodeItem);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(null);

        Element item = (Element) this.ratingEntry.clone();

        item.element("entry").element("target").element("id").detach();
        item.element("entry").element("target").addElement("id");
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.MISSING_TARGET_ID,
				validator.getErrorMessage());
	}

	@Test
	public void ifTargetedPostDoesntExistErrorIsReturned() throws Exception {

        NodeItem nodeItem = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
                .thenReturn(nodeItem);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
				.thenReturn(null);

        Element item = (Element) this.ratingEntry.clone();
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.TARGETED_ITEM_NOT_FOUND,
				validator.getErrorMessage());
	}

	@Test
	public void ifTargetedPostIsntInSameThreadErrorIsReturnedParentCheck()
			throws Exception {
        NodeItem nodeItem = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
                .thenReturn(nodeItem);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
                .thenReturn(nodeItem);

        Element item = (Element) this.ratingEntry.clone();
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.TARGET_MUST_BE_IN_SAME_THREAD,
				validator.getErrorMessage());
	}

	@Test
	public void ifTargetedPostIsntInSameThreadErrorIsReturnedThreadCheck()
			throws Exception {

        NodeItem nodeItem1 = new NodeItemImpl(node, "1", new Date(), "<entry/>");
        NodeItem nodeItem2 = new NodeItemImpl(node, "B", new Date(), "<entry/>",
				"A");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
                .thenReturn(nodeItem1);
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("2")))
                .thenReturn(nodeItem2);

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("B");
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.TARGET_MUST_BE_IN_SAME_THREAD,
				validator.getErrorMessage());
	}

	@Test
	public void ifTargetedIdIsSameAsReplyToIdOnlyOneDatabaseLookupPerformed()
			throws Exception {
        NodeItem nodeItem = new NodeItemImpl(node, "1", new Date(), "<entry/>");
		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node), Mockito.eq("1")))
                .thenReturn(nodeItem);

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");

        validator = getEntryObject(item);

		validator.isValid();

		Mockito.verify(channelManager, Mockito.times(1)).getNodeItem(
				Mockito.eq(node), Mockito.eq("1"));
	}

	@Test
	public void targetElementGetsAddedAsExpected() throws Exception {

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");

        validator = getEntryObject(item);

		Assert.assertTrue(validator.isValid());

		Element payload = validator.getPayload();

		String expectedId = "tag:channels.shakespeare.lit,/users/romeo@shakespeare.lit/posts,1";
		Assert.assertEquals(expectedId,
				payload.element("target").elementText("id"));
		Assert.assertEquals("post",
				payload.element("target").elementText("object-type"));
	}

	@Test
	public void missingInReplyToErrorsIfRatingElementPresent() throws Exception {

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("in-reply-to").detach();
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.IN_REPLY_TO_MISSING,
				validator.getErrorMessage());
	}

	@Test
	public void missingTargetErrorsIfRatingElementPresent() throws Exception {

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");
        item.element("entry").element("target").detach();
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.TARGET_ELEMENT_MISSING,
				validator.getErrorMessage());
	}

	@Test
	public void invalidRatingValueReturnsError() throws Exception {

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");
        item.element("entry").element("rating").setText("awesome");
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.INVALID_RATING_VALUE,
				validator.getErrorMessage());
	}

	@Test
	public void outOfRangeRatingReturnsError() throws Exception {

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("rating").setText("6.0");
        item.element("entry").element("target").element("id").setText("1");
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.RATING_OUT_OF_RANGE,
				validator.getErrorMessage());
	}

	@Test
	public void nonWholeNumberRatingReturnsError() throws Exception {
        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("rating").setText("4.1");
        item.element("entry").element("target").element("id").setText("1");
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.INVALID_RATING_VALUE,
				validator.getErrorMessage());
	}

	@Test
	public void ratingElementGetsAddedToPayloadAsExpected() throws Exception {

		String rating = "4";

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("rating").setText(rating);
        item.element("entry").element("target").element("id").setText("1");
        validator = getEntryObject(item);

		Assert.assertTrue(validator.isValid());
		Element payload = validator.getPayload();

		Assert.assertEquals(ActivityStreams.NS_REVIEW, payload
				.getNamespaceForPrefix("review").getText());
		Assert.assertEquals(rating + ".0", payload.element("rating")
				.getTextTrim());
	}

	@Test
	public void postTitleIsSetToRatingWhenRated() throws Exception {

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");
        validator = getEntryObject(item);

		Assert.assertTrue(validator.isValid());
		Element payload = validator.getPayload();

		Assert.assertEquals(ActivityStreams.NS_REVIEW, payload
				.getNamespaceForPrefix("review").getText());
		Assert.assertEquals("Rating", payload.elementText("title"));
	}

	@Test
	public void postVerbGetsSwitchedToRated() throws Exception {
        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");
        validator = getEntryObject(item);

		Assert.assertTrue(validator.isValid());
		Element payload = validator.getPayload();

        Assert.assertEquals(AtomEntry.ACTIVITY_VERB_RATED,
				payload.elementText("verb"));
	}

	@Test
	public void postContentGetsReplacedWithRating() throws Exception {
        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");

		String expectedContent = "rating:5.0";
        Assert.assertFalse(item.element("entry").elementText("content")
                .equals(expectedContent));

        validator = getEntryObject(item);

		Assert.assertTrue(validator.isValid());
		Element payload = validator.getPayload();

		Assert.assertEquals(expectedContent, payload.elementText("content"));
	}

	@Test
	public void canNotRateARating() throws Exception {

		String testPayload = "<entry xmlns=\"http://www.w3.org/2005/Atom\" "
				+ "xmlns:review=\"http://activitystrea.ms/schema/1.0/review\">"
				+ "<review:rating>5.0</review:rating></entry>";
        NodeItem nodeItem = new NodeItemImpl(node, "1", new Date(), testPayload);

		Mockito.when(
				channelManager.getNodeItem(Mockito.eq(node),
                        Mockito.anyString())).thenReturn(nodeItem);

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.CAN_ONLY_RATE_A_POST,
				validator.getErrorMessage());
	}

	@Test
	public void onlyAllowsSingleRatingPerPersonPerPost() throws Exception {
		Mockito.when(
                channelManager.userHasRatedPost(Mockito.anyString(),
                        Mockito.any(JID.class), Mockito.any(GlobalItemID.class)))
                .thenReturn(true);

        Element item = (Element) this.ratingEntry.clone();
        item.element("entry").element("target").element("id").setText("1");
        validator = getEntryObject(item);

		Assert.assertFalse(validator.isValid());
        Assert.assertEquals(AtomEntry.ITEM_ALREADY_RATED,
				validator.getErrorMessage());
	}

    @Test
    public void suppliesLocalItemId() throws Exception {
        Element item = (Element) this.publishEntry.clone();
        validator = getEntryObject(item);

        String localItemId = validator.getLocalItemId();

        Assert.assertNotNull(localItemId);
    }

    @Test
    public void suppliesGlobalItemId() throws Exception {
        Element item = (Element) this.publishEntry.clone();
        validator = getEntryObject(item);

        String globalItemId = validator.getGlobalItemId();

        Assert.assertNotNull(globalItemId);
        Assert.assertTrue(GlobalItemIDImpl.isGlobalId(globalItemId));
    }

    @Test
    public void suppliesInReplyTo() throws Exception {
        Assert.assertEquals(
                "tag:channels.shakespeare.lit,/users/romeo@shakespeare.lit/posts,fc362eb42085f017ed9ccd9c4004b095",
                replyEntry.element("entry").element("in-reply-to")
                        .attributeValue("ref"));

        NodeItem nodeItem = new NodeItemImpl(node, "2", new Date(), "<entry/>");

        Mockito.when(
                channelManager.getNodeItem(Mockito.eq(node),
                        Mockito.anyString())).thenReturn(nodeItem);

        Element item = (Element) this.replyEntry.clone();
        validator = getEntryObject(item);

        Assert.assertEquals("fc362eb42085f017ed9ccd9c4004b095",
                validator.getInReplyTo());
    }
    
    @Test
	public void addsMediaNamespace() throws Exception {
		Element entry = (Element) this.ratingEntry.clone();
		entry.element("target").element("id").setText("1");
		entry.addElement("media").addElement("item").addAttribute("channel", "marty@mcfly.com");

		validator = getEntryObject(entry);
		Assert.assertTrue(validator.isValid());
		Element payload = validator.getPayload();
		Assert.assertEquals(
			Buddycloud.NS_MEDIA,
			payload.getNamespaceForPrefix(Buddycloud.NS_MEDIA_PREFIX).getText()
		);
	}
	
	@Test
	public void existingMediaNamespaceDoesntCauseConflict() throws Exception {
		Element entry = (Element) this.ratingEntry.clone();
		entry.element("target").element("id").setText("1");
		entry.addNamespace(Buddycloud.NS_MEDIA_PREFIX, Buddycloud.NS_MEDIA);
		Element media = entry.addElement(Buddycloud.NS_MEDIA_PREFIX + ":media");
		media.addElement("item").addAttribute("channel", "marty@mcfly.com");

		validator = getEntryObject(entry);
		Assert.assertTrue(validator.isValid());
		Element payload = validator.getPayload();
		Assert.assertEquals(
			Buddycloud.NS_MEDIA,
			entry.getNamespaceForPrefix(Buddycloud.NS_MEDIA_PREFIX).getText()
		);
		Assert.assertNotNull(payload.element(Buddycloud.NS_MEDIA_PREFIX + ":media"));
	}
}